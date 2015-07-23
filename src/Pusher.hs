{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import System.Environment
import System.Console.GetOpt
import System.Pusher
import System.FilePath
import System.Exit
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Mime
import Aws
import Aws.S3 hiding (s3)
import Data.Text (pack)
import Pusher.Options
import Pusher.Operations
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
    -- Get our args and handle the options
    args <- getArgs

    case getOpt Permute options args of
        ([], _, []) -> error $ usageInfo header options
        (ss, _, []) -> start $ foldl (flip id) defaultOptions ss
        ( _, _, ms) -> error $ concat ms ++ usageInfo header options

start :: Options -> IO ()
start opts@Options{..} = do
    when optShowVersion $ putStrLn fullVersion

    -- Load our credentials
    mcreds <- loadAwsCreds $ pack optKeyName
    case mcreds of
        Just (fp, creds) -> do
            putStrLn $ concat [ "Found AWS creds with key '"
                              , optKeyName
                              , "' at " ++ fp ++ "/.aws-keys"
                              ]
            -- Create a new http manager for aws requests.
            mngr <- newManager tlsManagerSettings
            either putStrLn (\f -> f mngr creds) $ msum operations
                where operations = map ($ opts) [ uploadFile
                                                , nonOp
                                                ]

        Nothing -> do putStrLn $ concat [ "Could not find any AWS creds :(\n"
                                        , "Please add creds to this directory "
                                        , "or another parent directory."
                                        ]
                      exitFailure

nonOp :: Options -> Either String (Manager -> Credentials -> IO ())
nonOp _ = Right $ const $ const $ putStrLn "No operation specified."

uploadFile :: Options -> Either String (Manager -> Credentials -> IO ())
uploadFile Options{..} = do
    bucket <- check optBucket "No bucket specified"
    file   <- check optInput "No input file specified"
    let filename = takeFileName file
    name   <- msum [(</> filename) <$> (check optPath ""), Right filename ]
    acl    <- msum [check (optAcl >>= readAcl) "", Right AclPublicRead]
    return $ \mngr creds -> do
        lbs <- LBS.readFile file
        let r = mkPutObject buck ctype cenc acl name' lbs
            buck  = T.pack bucket
            name' = T.pack name
            ctype = (mkMime . T.pack) <$> optMime
            cenc  = T.pack <$> optEnc
        putStrLn $ unlines [ "Uploading " ++ file
                           , "  to bucket " ++ bucket
                           , "  with path " ++ name
                           ]
        rsp <- runS3 mngr creds r
        putStrLn $ show rsp

mkMime :: FileName -> MimeType
mkMime = mimeByExt defaultMimeMap defaultMimeType

check :: Maybe a -> String -> Either String a
check Nothing s = Left s
check (Just a) _ = Right a
