{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AwsOps where

import UserOps
import Network.HTTP.Conduit (RequestBody(..))
import Web.Scotty (File)
import Control.Monad
import Aws
import Aws.S3
import Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Parse as NWP
import qualified Data.ByteString as B

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

copyFile :: AwsCreds -> Bucket -> Text -> Text -> IO Text
copyFile creds bucket fromK toK = do
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

copyDirectory :: AwsCreds -> Bucket -> Text -> Text -> IO Text
copyDirectory creds bucket fromPrefix toPrefix = do
    infos <- listDirectory creds bucket (Just fromPrefix) (Just "/")
    forM_ infos $ \ObjectInfo{..} -> do
        let file   = T.drop (T.length fromPrefix) objectKey
            newKey = (if toPrefix == "/" then "" else toPrefix) `T.append` file
        putStrLn $ unpack $ T.unwords ["copying", objectKey, "to", newKey]
        unless (T.null newKey) $ void $ copyFile creds bucket objectKey newKey

    return "okay"
