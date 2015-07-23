{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent.Async
import System.Environment
import System.Console.GetOpt
import System.Console.ANSI
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
        (ss, fs, []) -> start fs $ foldl (flip id) defaultOptions ss
        ( _, _, ms) -> error $ concat ms ++ usageInfo header options

start :: [FilePath] -> Options -> IO ()
start files opts@Options{..} = do
    when optShowVersion $ putStrLn fullVersion

    mfiles <- mapM checkFilePath files
    fs <- case sequence mfiles of
        Nothing -> do putStrLn $ unlines $ "Some input files do not exist:":
                                           map (("  " ++) . show) mfiles
                      exitFailure
        Just fs -> return fs

    let opts' = opts{ optFiles = fs }

    -- Load our credentials
    mcreds <- loadAwsCreds $ pack optKeyName
    case mcreds of
        Just (fp, creds) -> do
            putStrLn $ concat [ "Found AWS creds with key '"
                              , optKeyName
                              , "' at " ++ fp
                              ]
            -- Create a new http manager for aws requests.
            mngr <- newManager tlsManagerSettings
            either putStrLn (\f -> f mngr creds) $ msum operations
                where operations = map ($ opts') [ uploadFiles
                                                 , nonOp
                                                 ]

        Nothing -> do putStrLn $ concat [ "Could not find any AWS creds :(\n"
                                        , "Please add creds to this directory "
                                        , "or another parent directory."
                                        ]
                      exitFailure

nonOp :: Options -> Either String (Manager -> Credentials -> IO ())
nonOp _ = Right $ const $ const $ putStrLn "No operation specified."

uploadFiles :: Options -> Either String (Manager -> Credentials -> IO ())
uploadFiles Options{..}
    | []      <- optFiles  = Left "No input files specified"
    | Nothing <- optBucket = Left "No bucket specified"
    | Just bucket <- optBucket = do
        acl    <- msum [check (optAcl >>= readAcl) "", Right AclPublicRead]
        path   <- msum [check optPath "", Right ""]
        return $ \mngr creds -> do
            void $ (flip mapConcurrently) optFiles $ \file -> do
                let buck  = T.pack bucket
                    ctype = (mkMime . T.pack) <$> optMime
                    cenc  = T.pack <$> optEnc
                    gz    = not optIsGzip
                uploadFile buck ctype cenc acl (T.pack path) file gz mngr creds

    | otherwise = Left "Unknown option configuration."

mkMime :: FileName -> MimeType
mkMime = mimeByExt defaultMimeMap "text/plain"

check :: Maybe a -> String -> Either String a
check Nothing s = Left s
check (Just a) _ = Right a

