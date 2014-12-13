{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AwsOps where

import UserOps
import Network.HTTP.Client
import Web.Scotty (File)
import Aws
import Aws.S3
import Data.Text as T
import Control.Monad
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Parse as NWP
import qualified Data.ByteString as B

data FileAccess = FileAccess { faCreds  :: AwsCreds
                             , faBucket :: Bucket
                             , faKey    :: Text
                             } deriving (Show, Eq)

getFile :: FileAccess -> IO GetObjectMemoryResponse
getFile (FileAccess creds bucket key) = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        r    = getObject bucket key
    simpleAws cfg scfg r

uploadFile :: AwsCreds -> Bucket -> Maybe B.ByteString -> Maybe Text
           -> CannedAcl -> File -> IO Text
uploadFile creds buck ctype cenc acl f = do
    awsCreds <- makeCredentials (awsKey creds) (awsSecret creds)
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp awsCreds $ defaultLog Warning
        bdy  = RequestBodyLBS $ NWP.fileContent $ snd f
        name = LT.toStrict $ fst f
        r    = (putObject buck name bdy){ poContentType = ctype
                                        , poContentEncoding = cenc
                                        , poAcl = Just acl
                                        }
    _ <- simpleAws cfg scfg r
    return "okay"

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
