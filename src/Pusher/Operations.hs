{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Pusher.Operations where

import Aws
import Aws.S3 hiding (s3)
import Network.HTTP.Conduit
import Network.Mime
import Control.Monad.Trans.Resource
import Control.Exception
import Control.Monad
import System.Console.ANSI
import System.FilePath
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS

uploadFile :: Bucket -- ^ The destination bucket name.
           -> Maybe B.ByteString -- ^ The content type.
           -> Maybe Text -- ^ The content encoding.
           -> CannedAcl -- ^ The canned permissions.
           -> Text -- ^ The destination path/key/name prefix of the file inside
                   --   the bucket.
           -> FilePath -- ^ The path to the local file.
           -> Bool -- ^ Whether or not to strip the .gz extension and set
                   --   content type to "gzip" when a gzip extension is
                   --   found.
           -> Manager -> Credentials -> IO ()
uploadFile buck ctype cenc acl pfx file gz mngr creds = do
    lbs <- LBS.readFile file
    let r = mkPutObject buck mtyp menc' acl (T.pack name) lbs
        name = (T.unpack pfx) </> takeFileName file'
        menc' = (msum [menc, cenc])
        (menc,file') = if (".gz" `L.isSuffixOf` file) && gz
                       then (Just "gzip", dropExtension file)
                       else (Nothing, file)
        mtyp = msum [ctype
                    ,Just $ mimeByExt defaultMimeMap defaultMimeType $ T.pack file'
                    ]
    rsp <- runS3 mngr creds r
    case rsp of
        Left err -> do withSGR [SetColor Foreground Dull Red] $ do
                           putStr "☒ "
                           putStrLn $ unwords [file, "->"
                                              , T.unpack buck </> name
                                              ]
                       putStrLn $ "  (" ++ show err ++ ")"
        Right _  -> do withSGR [SetColor Foreground Dull Green] $ do
                           putStr "☑ "
                           putStrLn $ unwords [file
                                              , "->"
                                              , T.unpack buck </> name
                                              ]
                       let info = L.intercalate "\n  "
                                                [ "bucket: " ++ show buck
                                                , "encoding: " ++ show menc
                                                , "type: " ++ show mtyp
                                                , "acl: " ++ show acl
                                                , "prefix: " ++ show pfx
                                                , "name: " ++
                                                    (show $ takeFileName file')
                                                ]
                       putStrLn $ "  " ++ info ++ "\n"

withSGR :: [SGR] -> IO () -> IO ()
withSGR codes f = do
    setSGR codes
    f
    setSGR []

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
runS3 mngr creds r =
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp creds $ defaultLog Warning
    in try $ runResourceT $ pureAws cfg scfg mngr r

