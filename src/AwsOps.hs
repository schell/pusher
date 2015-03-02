{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AwsOps where

import Prelude as P
import Types
import Network.HTTP.Conduit
import Network.Mime
import Web.Scotty (File)
import Aws
import Aws.S3
import Data.Text as T
import Control.Monad
import Control.Monad.Trans.Resource
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

getFile :: FileAccess -> IO GetObjectMemoryResponse
getFile (FileAccess creds bucket key) = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        r    = getObject bucket key
    simpleAws cfg scfg r

-- | Uploads one file to one specific place on s3.
uploadFile :: Manager -> AwsCreds -> Bucket -> Maybe B.ByteString -> Maybe Text
           -> CannedAcl -> Maybe Text -> File -> IO (Either S3Error PutObjectResponse)
uploadFile mngr creds buck ctype cenc acl mkey f = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        bdy  = RequestBodyLBS $ NWP.fileContent $ snd f
        name = maybe (LT.toStrict $ fst f) id mkey
        r    = (putObject buck name bdy){ poContentType = ctype
                                        , poContentEncoding = cenc
                                        , poAcl = Just acl
                                        }
    try $ runResourceT $ pureAws cfg scfg mngr r

updateTaskOutput :: TasksVar -> B.ByteString -> UniqueID -> IO ()
updateTaskOutput tvar bs = atomically . modifyTVar' tvar . M.adjust (bs `B.append`)

-- | Uploads a zip of a bunch of files and directories into their
-- corresponding places on s3. Proxies files to uploadFile.
uploadZippedDir :: Manager -> FilePath -> UniqueID -> TasksVar -> AwsCreds
                -> Bucket -> CannedAcl -> Text -> File -> IO ()
uploadZippedDir mngr tmp uid tvar creds buck acl key f = do
    atomically $ modifyTVar' tvar $ M.insert uid "Starting expansion and uploads..."
    void $ forkIO $ do
        -- Create a temp dir to hold our zip
        let zbin  = NWP.fileContent $ snd f
            zdir  = tmp </> "pusher" ++ show uid
            zextr = zdir </> "unzipped"
        createDirectoryIfMissing True $ zextr

        -- Write the zip to disk
        let zname = zdir </> (LT.unpack $ fst f)
        putStrLn $ "Writing " ++ zname
        LBS.writeFile zname zbin

        -- List all the files in the zip
        fs <- readProcess "tar" ["tf", zname] []
        let fs'  = P.filter (not . ("/" `L.isSuffixOf`)) $ P.lines fs
            fs'' = if ("./" `L.isPrefixOf` P.head fs')
                     then P.map (P.drop 2) fs'
                     else fs'

        -- Run through each file and upload it to s3 along with its mimetype,
        -- encoding and key. Assume that gzipped files are meant to be served
        -- as gzipped encoding.
        let len = P.length fs''
            is = [0..] :: [Int]
        success <- forM (P.zip is fs'') $ \(i, file) -> do
            (code, _, _) <- readProcessWithExitCode "tar" ["xf", zname, "-C", zextr, file] []
            case code of
                ExitSuccess -> do lbsf <- LBS.readFile $ zextr </> file
                                  let (menc,key') = if ".gz" `L.isSuffixOf` file
                                                    then (Just "gzip", dropExtension file)
                                                    else (Nothing, file)
                                      mime = mimeByExt defaultMimeMap defaultMimeType $ T.pack key'
                                      finfo = NWP.FileInfo "file" mime lbsf
                                      nfile = ("file", finfo)
                                      k  = T.pack $ T.unpack key </> key'
                                      msg = P.unwords [ "(" ++ show i ++ "/" ++ show len ++ ")"
                                                      , "Uploaded"
                                                      , T.unpack k
                                                      , "\n"
                                                      ]
                                  eIO <- uploadFile mngr creds buck (Just mime) menc acl (Just k) nfile
                                  let (msg', s) = case eIO of
                                                      Left err -> (B.pack $ show err, 0)
                                                      Right _  -> (B.pack msg, 1)
                                  updateTaskOutput tvar msg' uid
                                  return s
                ExitFailure _ -> return 0
        let successes = sum success
            msg = P.unwords [ "Complete. Uploaded"
                            , show (successes :: Int)
                            , "of"
                            , show len
                            , "files.\n"
                            ]
        updateTaskOutput tvar (B.pack msg) uid

-- | Copies a file from anywhere in S3 to anywhere in S3.
copyFile :: FileAccess -> FileAccess -> IO Text
copyFile f@(FileAccess _ fbucket _) (FileAccess tcreds tbucket tkey)
    | fbucket == tbucket = copyFileInBucket f tkey
    | otherwise = do
       GetObjectMemoryResponse md response <- getFile f
       tawsCreds <- makeCredentials (awsKey tcreds) (awsSecret tcreds)
       let scfg = defServiceConfig :: S3Configuration NormalQuery
           cfg  = Configuration Timestamp tawsCreds $ defaultLog Warning
           r    = putObject tbucket tkey (RequestBodyLBS $ responseBody response)
           r'   = r{ poMetadata = omUserMetadata md }
       _ <- simpleAws cfg scfg r'
       return "okay"

-- | Performs a copy within one bucket.
copyFileInBucket :: FileAccess -> Text -> IO Text
copyFileInBucket (FileAccess creds bucket fromK) toK = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let from = ObjectId bucket fromK Nothing
        scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        r    = copyObject bucket toK from CopyMetadata
    _ <- simpleAws cfg scfg r
    return "okay"

listDirectory :: AwsCreds -> Bucket -> Maybe Text -> Maybe Text -> IO [ObjectInfo]
listDirectory creds buck mprefix mdelim = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        r = (getBucket buck) { gbDelimiter = mdelim
                             , gbPrefix = mprefix
                             }
    fmap gbrContents $ simpleAws cfg scfg r

copyDirectory :: FileAccess -> FileAccess -> IO Text
copyDirectory f@(FileAccess fcreds fbucket fpre) (FileAccess tcreds tbucket tpre)
    | fbucket == tbucket = copyDirectoryInBucket f tpre
    | otherwise = do
        let mfpre = if fpre == "/" then Nothing else Just fpre
        infos <- listDirectory fcreds fbucket mfpre (Just "/")
        putStrLn "got list of (from) bucket"
        print infos
        forM_ infos $ \ObjectInfo{..} -> do
            let n = if fpre == "/" then 0 else T.length fpre
                file = T.drop n objectKey
                newKey = (if tpre == "/" then "" else tpre) `T.append` file
            putStrLn $ unpack $ T.unwords ["copying", objectKey, "to", newKey]
            unless (T.null newKey) $ do
                void $ copyFile (FileAccess fcreds fbucket objectKey)
                                (FileAccess tcreds tbucket newKey)
        return "okay"

copyDirectoryInBucket :: FileAccess -> Text -> IO Text
copyDirectoryInBucket (FileAccess creds bucket fpre) tpre = do
    let mfpre = if fpre == "/" then Nothing else Just fpre
    infos <- listDirectory creds bucket mfpre (Just "/")
    forM_ infos $ \ObjectInfo{..} -> do
        let n = if fpre == "/" then 0 else T.length fpre
            file = T.drop n objectKey
            newKey = (if tpre == "/" then "" else tpre) `T.append` file
        putStrLn $ unpack $ T.unwords ["copying", objectKey, "to", newKey]
        unless (T.null newKey) $
            void $ copyFileInBucket (FileAccess creds bucket objectKey) newKey
    return "okay"
