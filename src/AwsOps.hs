{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AwsOps where

import Prelude as P
import Types
import Network.HTTP.Client
import Network.Mime
import Web.Scotty (File)
import Aws
import Aws.S3
import Data.Text as T
import Control.Monad
import Control.Applicative
import System.Directory hiding (copyFile)
import System.FilePath
import System.Process
import System.Exit
import qualified Data.List as L
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Parse as NWP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS

getFile :: FileAccess -> IO GetObjectMemoryResponse
getFile (FileAccess creds bucket key) = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        r    = getObject bucket key
    simpleAws cfg scfg r

-- | Uploads one file to one specific place on s3.
uploadFile :: AwsCreds -> Bucket -> Maybe B.ByteString -> Maybe Text
           -> CannedAcl -> Maybe Text -> File -> IO Text
uploadFile creds buck ctype cenc acl mkey f = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        bdy  = RequestBodyLBS $ NWP.fileContent $ snd f
        name = maybe (LT.toStrict $ fst f) id mkey
        r    = (putObject buck name bdy){ poContentType = ctype
                                        , poContentEncoding = cenc
                                        , poAcl = Just acl
                                        }
    _ <- simpleAws cfg scfg r
    return "okay"


-- | Uploads a zip of a bunch of files and directories into their
-- corresponding places on s3. Proxies files to uploadFile.
uploadZippedDir :: FilePath -> UniqueID -> AwsCreds -> Bucket -> CannedAcl -> Text -> File -> IO Text
uploadZippedDir tmp uid creds buck acl key f = do
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
    fs <- P.filter (not . ("/" `L.isSuffixOf`)) . P.lines <$>
              readProcess "tar" ["tf", zname] []

    -- Run through each file and upload it to s3 along with its mimetype,
    -- encoding and key. Assume that gzipped files are meant to be served
    -- as gzipped encoding.
    xs <- forM fs $ \file -> do
        (code, _, err) <- readProcessWithExitCode "tar" ["xf", zname, "-C", zextr, file] []
        case code of
            ExitSuccess -> do lbsf <- LBS.readFile $ zextr </> file
                              let (menc,key') = if ".gz" `L.isSuffixOf` file
                                                then (Just "gzip", dropExtension file)
                                                else (Nothing, file)
                                  mime = mimeByExt defaultMimeMap defaultMimeType $ T.pack key'
                                  finfo = NWP.FileInfo "file" mime lbsf
                                  nfile = ("file", finfo)
                                  mkey  = Just $ T.pack $ T.unpack key </> key'
                              uploadFile creds buck (Just mime) menc acl mkey nfile
            ExitFailure _ -> return $ T.unwords [ "Could not process"
                                                , T.pack file
                                                , ":"
                                                , T.pack err
                                                ]
    return $ T.intercalate "\n" $ xs

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
