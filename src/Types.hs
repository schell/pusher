{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types where

import Prelude as P
import Aws.S3
import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import Web.Scotty.Trans hiding (get, post)
import Data.Time.Clock
import Data.Configurator.Types
import Data.Text as T
import Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT

newtype UniqueID = UniqueID Int
type UIDVar = TVar UniqueID

instance Show UniqueID where
    show (UniqueID uid) = show uid

instance Enum UniqueID where
    fromEnum (UniqueID uid) = uid
    toEnum = UniqueID

data FileAccess = FileAccess { faCreds  :: AwsCreds
                             , faBucket :: Bucket
                             , faKey    :: Text
                             } deriving (Show, Eq)

data AwsCreds = AwsCreds { awsKey :: ByteString
                         , awsSecret :: ByteString
                         } deriving (Show, Read, Eq)

type UserName = Text

data UserDetail = UserDetail { userLevel :: Int
                             , userName  :: UserName
                             , userPass  :: ByteString
                             , userCreds :: M.Map Bucket AwsCreds
                             } deriving (Show, Read, Eq)

data UserCookie = UserCookie UserDetail UTCTime deriving (Show, Read, Eq)

type Users = M.Map UserName UserDetail
type UsersVar = TVar Users

data LogEntry = LogEntry { logUser   :: UserName
                         , logTime   :: UTCTime
                         , logParams :: [Param]
                         , logPath   :: Text
                         } deriving (Show, Read)
newtype Log = Log [LogEntry]
type LogVar = TVar Log
data Pusher = Pusher { pLogVar   :: LogVar
                     , pUsersVar :: UsersVar
                     , pConfig   :: Config
                     , pNextID   :: UIDVar
                     }
type ActionP = ActionT LT.Text (ReaderT Pusher IO)
type ScottyP = ScottyT LT.Text (ReaderT Pusher IO)
