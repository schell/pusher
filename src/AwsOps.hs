{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module AwsOps where

import Prelude as P
import Types
import Routes.Utils
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (mkStatus)
import Network.Mime
import Web.Scotty (File)
import Aws
import Aws.S3 hiding (s3)
import Data.Text as T
import Data.Time.Clock
import Data.Either
import Data.Conduit
import Data.Conduit.Binary
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import System.Directory hiding (copyFile)
import System.FilePath
import System.Process
import System.Exit
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Parse as NWP
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS

--------------------------------------------------------------------------------
-- S3 calls and helpers
--------------------------------------------------------------------------------
s3IO :: (Transaction r a, ServiceConfiguration r ~ S3Configuration)
     => Manager -> AwsCreds -> r -> IO (Either S3Error a)
s3IO mngr AwsCreds{..} r = do
    creds <- liftIO $ makeCredentials awsKey awsSecret
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp creds $ defaultLog Warning
    try $ runResourceT $ pureAws cfg scfg mngr r

s3 :: (Transaction r a, ServiceConfiguration r ~ S3Configuration)
   => AwsCreds -> r -> ActionP (Either S3Error a)
s3 c r = do
    m <- getManager
    liftIO $ s3IO m c r

fakeS3Error :: S3Error
fakeS3Error = S3Error { s3StatusCode = mkStatus 0 ""
                      , s3ErrorCode  = ""
                      , s3ErrorMessage = ""
                      , s3ErrorResource = Nothing
                      , s3ErrorHostId = Nothing
                      , s3ErrorAccessKeyId = Nothing
                      , s3ErrorStringToSign = Nothing
                      }

mkPutObject :: Bucket -> Maybe B.ByteString -> Maybe Text -> CannedAcl
            -> Maybe Text -> File -> PutObject
mkPutObject buck ctype cenc acl mkey f =
    let bdy  = RequestBodyLBS $ NWP.fileContent $ snd f
        name = maybe (LT.toStrict $ fst f) id mkey
    in (putObject buck name bdy){ poContentType = ctype
                                , poContentEncoding = cenc
                                , poAcl = Just acl
                                }

--------------------------------------------------------------------------------
-- IO Task handling
--------------------------------------------------------------------------------
insertPendingTask :: TasksVar -> UniqueId -> IO ()
insertPendingTask tvar uid =
    atomically $ modifyTVar' tvar $ M.insert uid $ PendingTask []

updatePendingTask :: TasksVar -> UniqueId -> TaskUpdate -> IO ()
updatePendingTask tvar uid t = atomically $ modifyTVar' tvar $ M.adjust f uid
    where f (PendingTask ts) = PendingTask $ t:ts
          f x = x

checkTaskStatus :: TasksVar -> UniqueId -> (Bucket, Maybe Text) -> Int -> IO ()
checkTaskStatus tvar uid bmcf n = do
    tasks <- atomically $ readTVar tvar
    case M.lookup uid tasks of
        Just (PendingTask tus) -> if P.length tus < n
                                  then return ()
                                  else finalizeTaskSuccess tvar uid bmcf
        _ -> return ()

finalizeTaskSuccess :: TasksVar -> UniqueId -> (Bucket, Maybe Text) -> IO ()
finalizeTaskSuccess tvar uid bcf = do
    t <- getCurrentTime
    atomically $ modifyTVar' tvar $ M.adjust (completeTask bcf t) uid

finalizeTaskFail :: TasksVar -> UniqueId -> IO ()
finalizeTaskFail tvar uid = do
    t <- getCurrentTime
    atomically $ modifyTVar' tvar $ M.adjust (failTask t) uid
    --
--------------------------------------------------------------------------------
-- IO Helpers
--------------------------------------------------------------------------------
getFileIO :: Manager -> FileAccess -> IO (Either S3Error GetObjectResponse)
getFileIO mngr (FileAccess creds bucket key) =
    s3IO mngr creds $ getObject bucket key

-- | Uploads one file to one specific place on s3.
uploadFileIO :: Manager -> AwsCreds -> Bucket -> Maybe B.ByteString
             -> Maybe Text -> CannedAcl -> Maybe Text -> File
             -> IO (Either S3Error PutObjectResponse)
uploadFileIO mngr creds b mime enc acl k f =
    s3IO mngr creds $ mkPutObject b mime enc acl k f

-- | Copies a file from anywhere in S3 to anywhere in S3.
copyFileIO :: Manager -> FileAccess -> FileAccess
           -> IO (Either S3Error PutObjectResponse)
copyFileIO mngr f (FileAccess tcreds tbucket tkey) = do
   mres <- getFileIO mngr f
   case mres of
       Left err -> return $ Left err
       Right GetObjectResponse{..} -> do
           let src = responseBody gorResponse
           lbs <- runResourceT $ src $$+- sinkLbs
           let r    = putObject tbucket tkey (RequestBodyLBS lbs)
               r'   = r{ poMetadata = omUserMetadata gorMetadata }
           s3IO mngr tcreds r'

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------
getFile :: FileAccess -> ActionP (Either S3Error GetObjectResponse)
getFile (FileAccess creds bucket key) = s3 creds $ getObject bucket key

copyFile :: FileAccess -> FileAccess
         -> ActionP (Either S3Error PutObjectResponse)
copyFile f t = do
    mngr <- getManager
    liftIO $ copyFileIO mngr f t

-- | Uploads one file to one specific place on s3.
-- Creates a new task to upload the file (it may be a biggy).
uploadFile :: AwsCreds -> Bucket -> Maybe B.ByteString -> Maybe Text
           -> CannedAcl -> Maybe Text -> File -> ActionP UniqueId
uploadFile creds b mime enc acl k f = do
    uid  <- newUniqueId
    tvar <- getTasksVar
    mngr <- getManager
    mcf  <- getCloudfrontDistroForBucket b
    liftIO $ insertPendingTask tvar uid
    let po@PutObject{..} = mkPutObject b mime enc acl k f
    void $ liftIO $ forkIO $ do
        epo <- s3IO mngr creds po
        case epo of
            Left err -> do updatePendingTask tvar uid $ TaskError 0 "" $ show err
                           finalizeTaskFail tvar uid
            Right _  -> do updatePendingTask tvar uid $ TaskSuccess $ T.unpack poObjectName
                           finalizeTaskSuccess tvar uid (b, mcf)
    return uid

-- | Uploads a zip of a bunch of files and directories into their
-- corresponding places on s3. Proxies files to uploadFileIO.
uploadZippedDir :: AwsCreds -> Bucket -> CannedAcl -> Text -> File
                -> ActionP (UniqueId, [FilePath])
uploadZippedDir creds buck acl key f = do
    tmp  <- getTempDir
    mngr <- getManager
    mcf  <- getCloudfrontDistroForBucket buck
    tvar <- getTasksVar
    uid  <- newUniqueId

    let zbin  = NWP.fileContent $ snd f
        zdir  = tmp </> "pusher" ++ show uid
        zextr = zdir </> "unzipped"
        zname = zdir </> (LT.unpack $ fst f)
    files <- liftIO $ do
        -- Create a temp dir to hold our zip
        createDirectoryIfMissing True $ zextr

        -- Write the zip to disk
        putStrLn $ "Writing " ++ zname
        LBS.writeFile zname zbin

        -- List all the files in the zip
        fs <- readProcess "tar" ["tf", zname] []
        let fs'  = P.filter (not . ("/" `L.isSuffixOf`)) $ P.lines fs
            fs'' = P.map (\l -> if "./" `L.isPrefixOf` l then P.drop 2 l else l) fs'
        return $ P.zip fs'' fs'



    liftIO $ atomically $ modifyTVar' tvar $ M.insert uid $ PendingTask []
    liftIO $ forM_ files $ \(file, tfile) -> forkIO $ do
        -- Run through each file and upload it to s3 along with its mimetype,
        -- encoding and key. Assume that gzipped files are meant to be served
        -- as gzipped encoding.
        (code, stdout, stderr) <- readProcessWithExitCode "tar" ["xzf", zname, "-C", zextr, tfile] []
        case code of
            ExitSuccess -> do lbsf <- LBS.readFile $ zextr </> file
                              let (menc,key') = if ".gz" `L.isSuffixOf` file
                                                then (Just "gzip", dropExtension file)
                                                else (Nothing, file)
                                  mime = mimeByExt defaultMimeMap defaultMimeType $ T.pack key'
                                  finfo = NWP.FileInfo "file" mime lbsf
                                  nfile = ("file", finfo)
                                  k  = T.pack $ T.unpack key </> key'
                              eIO <- uploadFileIO mngr creds buck (Just mime) menc acl (Just k) nfile
                              let tu = case eIO of
                                           Left err -> TaskError 0 "" $ show err
                                           Right _  -> TaskSuccess $ T.unpack k
                              updatePendingTask tvar uid tu
                              checkTaskStatus tvar uid (buck, mcf) $ P.length files
            ExitFailure e -> do let tu = TaskError e stdout stderr
                                updatePendingTask tvar uid tu
                                checkTaskStatus tvar uid (buck, mcf) $ P.length files
    return (uid, P.map fst files)

-- | Performs a copy within one bucket.
copyFileInBucket :: FileAccess -> Text -> ActionP (Either S3Error CopyObjectResponse)
copyFileInBucket (FileAccess creds bucket fromK) toK = do
    let from = ObjectId bucket fromK Nothing
        r    = copyObject bucket toK from CopyMetadata
    s3 creds r

listDirectory :: AwsCreds -> Bucket -> Maybe Text -> Maybe Text
              -> ActionP (Either S3Error GetBucketResponse)
listDirectory creds buck mprefix mdelim =
    s3 creds $ (getBucket buck) { gbDelimiter = mdelim
                                , gbPrefix = mprefix
                                }

copyDirectory :: FileAccess -> FileAccess
              -> ActionP (Either S3Error UniqueId)
copyDirectory (FileAccess fcreds fbucket fpre) (FileAccess tcreds tbucket tpre) = do
    let mfpre = if fpre == "/" then Nothing else Just fpre
    egbr <- listDirectory fcreds fbucket mfpre Nothing
    case egbr of
        Left err -> return $ Left err
        Right GetBucketResponse{..} -> do
            uid  <- newUniqueId
            tvar <- getTasksVar
            mngr <- getManager
            mcf  <- getCloudfrontDistroForBucket tbucket
            liftIO $ insertPendingTask tvar uid

            void $ liftIO $ forM gbrContents $ \ObjectInfo{..} -> forkIO $ do
                let n = if fpre == "/" then 0 else T.length fpre
                    file = T.drop n objectKey
                    newKey = (if tpre == "/" then "" else tpre) `T.append` file
                if (T.null newKey)
                then return ()
                else do ecor <- copyFileIO mngr
                                           (FileAccess fcreds fbucket objectKey)
                                           (FileAccess tcreds tbucket newKey)
                        let tu = case ecor of
                                     Left err -> TaskError 0 "" $ show err
                                     Right _ -> TaskSuccess $ T.unpack newKey
                        updatePendingTask tvar uid tu
                        checkTaskStatus tvar uid (tbucket, mcf) $ P.length gbrContents
            return $ Right uid

copyDirectoryInBucket :: FileAccess -> Text -> ActionP (Either S3Error [CopyObjectResponse])
copyDirectoryInBucket (FileAccess creds bucket fpre) tpre = do
    let mfpre = if fpre == "/" then Nothing else Just fpre
    egbr <- listDirectory creds bucket mfpre (Just "/")
    case egbr of
        Left err -> return $ Left err
        Right GetBucketResponse{..} -> do
            copies <- forM gbrContents $ \ObjectInfo{..} -> do
                let n = if fpre == "/" then 0 else T.length fpre
                    file = T.drop n objectKey
                    newKey = (if tpre == "/" then "" else tpre) `T.append` file
                if (T.null newKey)
                then return $ Left fakeS3Error
                else copyFileInBucket (FileAccess creds bucket objectKey) newKey
            return $ Right $ rights copies
