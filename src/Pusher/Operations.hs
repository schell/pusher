{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Pusher.Operations where

import Prelude as P
import Network.HTTP.Conduit
import Aws
import Aws.S3 hiding (s3)
import Data.Text as T
import Control.Monad.Trans.Resource
import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS

mkPutObject :: Bucket -> Maybe B.ByteString -> Maybe Text -> CannedAcl
            -> Text -> LBS.ByteString -> PutObject
mkPutObject buck ctype cenc acl name lbs =
    let bdy = RequestBodyLBS lbs in
    (putObject buck name bdy){ poContentType = ctype
                             , poContentEncoding = cenc
                             , poAcl = Just acl
                             }

runS3 :: (Transaction r a, ServiceConfiguration r ~ S3Configuration)
      => Manager -> Credentials -> r -> IO (Either S3Error a)
runS3 mngr creds r = do
    putStrLn "Running S3"
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp creds $ defaultLog Warning
    try $ runResourceT $ pureAws cfg scfg mngr r
