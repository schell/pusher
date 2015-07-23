{-# LANGUAGE RecordWildCards #-}
module Pusher.Options where

import System.Console.GetOpt
import System.Info
import qualified Config
import Data.Version (showVersion)
import Paths_pusher (version)
import Aws.S3.Core

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optKeyName = "default"
                         , optBucket = Nothing
                         , optPath = Nothing
                         , optFiles = []
                         , optMime = Nothing
                         , optAcl = Just "AclPublicRead"
                         , optEnc = Nothing
                         , optOperation = Nothing
                         , optIsGzip = False
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["version"]
              (NoArg (\opts -> opts{ optShowVersion = True }))
              "Show version info."
          , Option "k" ["keyname"]
              (ReqArg (\s opts -> opts{ optKeyName = s }) "keyname")
              "AWS credential key name."
          , Option "b" ["bucket"]
              (ReqArg (\s opts -> opts{ optBucket = Just s }) "bucket")
              "Destination bucket."
          , Option "p" ["path"]
              (ReqArg (\s opts -> opts{ optPath = Just s }) "path")
              "Destination path."
          , Option "" ["mime"]
              (ReqArg (\s opts -> opts{ optMime = Just s }) "mime")
              "Destination mime-type."
          , Option "" ["enc"]
              (ReqArg (\s opts -> opts{ optMime = Just s }) "enc")
              "Destination file encoding."
          , Option "" ["acl"]
              (ReqArg (\s opts -> opts{ optAcl = Just s }) "acl")
              "Destination canned ACL."
          , Option "z" ["gzip"]
              (NoArg (\opts -> opts{ optIsGzip = True }))
              $ unwords [ "Uploaded files should maintain their .gz extension"
                        , "and be served from S3 with a content-type of"
                        , "application/x-gzip and will not be served gzip"
                        , "encoded."
                        ]
          , Option "o" ["op"]
              (ReqArg (\s opts -> opts{ optOperation = readOperation s }) "op")
              (unwords $ "Operation command. Valid values are" : vals)
          ]
    where vals = map showOperation [ OperationUploadFile ]

header :: String
header = "Usage: pusher [options] INPUT_FILE1 INPUT_FILE2 INPUT_FILE3 ..."

fullVersion :: String
fullVersion = concat [ programVersion
                     , " ("
                     , "ghc-", Config.cProjectVersion, "-", arch, "-", os
                     , ")"
                     ]

programVersion :: String
programVersion = "pusher v" ++ showVersion version

data Options = Options { optShowVersion :: Bool
                       , optKeyName     :: String
                       , optBucket      :: Maybe String
                       , optPath        :: Maybe String
                       , optFiles       :: [FilePath]
                       , optMime        :: Maybe String
                       , optEnc         :: Maybe String
                       , optAcl         :: Maybe String
                       , optOperation   :: Maybe Operation
                       , optIsGzip      :: Bool
                       } deriving (Show, Eq)

readAcl :: String -> Maybe CannedAcl
readAcl "AclPrivate" = Just AclPrivate
readAcl "AclPublicRead" = Just AclPublicRead
readAcl "AclPublicReadWrite" = Just AclPublicReadWrite
readAcl "AclAuthenticatedRead" = Just AclAuthenticatedRead
readAcl "AclBucketOwnerRead" = Just AclBucketOwnerRead
readAcl "AclBucketOwnerFullControl" = Just AclBucketOwnerFullControl
readAcl "AclLogDeliveryWrite" = Just AclLogDeliveryWrite
readAcl _ = Nothing

readOperation :: String -> Maybe Operation
readOperation "upload-file" = Just OperationUploadFile
readOperation _ = Nothing

showOperation :: Operation -> String
showOperation OperationUploadFile = "upload-file"

data Operation = OperationUploadFile deriving (Show, Eq)
