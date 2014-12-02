{-# LANGUAGE OverloadedStrings #-}
module AwsOps where

import UserOps
import Web.Scotty
import Control.Monad.IO.Class
import Network.HTTP.Conduit (RequestBody(..))
import Aws
import Aws.S3
import Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Parse as NWP
import qualified Data.ByteString as B

uploadFile :: Bucket -> AwsCreds -> Maybe B.ByteString -> Maybe Text -> CannedAcl -> File -> IO Text
uploadFile buck creds ctype cenc acl f = do
    awsCreds <- liftIO $ makeCredentials (awsKey creds) (awsSecret creds)
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
