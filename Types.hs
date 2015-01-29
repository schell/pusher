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
import Data.Typeable
import Data.Text as T
import Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT

data FileAccess = FileAccess { faCreds  :: AwsCreds
                             , faBucket :: Bucket
                             , faKey    :: Text
                             } deriving (Show, Eq)

data AwsCreds = AwsCreds { awsKey :: ByteString
                         , awsSecret :: ByteString
                         } deriving (Show, Read, Eq)

type UserName = Text

data UserDetail = UserDetail { userLevel :: Int
                             , userPass  :: ByteString
                             , userCreds :: M.Map Bucket AwsCreds
                             } deriving (Show, Read, Typeable)

type Users = M.Map UserName UserDetail
type UsersVar = TVar Users


data LogEntry = LogEntry { logTime   :: UTCTime
                         , logParams :: [Param]
                         , logPath   :: Text
                         } deriving (Show, Read)
newtype Log = Log [LogEntry] deriving (Typeable)
type LogVar = TVar Log
deriving instance Typeable Config
data Pusher = Pusher { pLogVar   :: LogVar
                     , pUsersVar :: UsersVar
                     , pConfig   :: Config
                     }
type ActionP = ActionT LT.Text (ReaderT Pusher IO)
type ScottyP = ScottyT LT.Text (ReaderT Pusher IO)
