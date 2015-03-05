{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types where

import Prelude as P
import Aws.S3
import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import Web.Scotty.Trans hiding (get, post)
import Network.HTTP.Client
import Data.Time.Clock
import Data.Configurator.Types
import Data.Text as T
import Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT

newtype UniqueId = UniqueId Int deriving (Eq, Ord)
type UIDVar = TVar UniqueId

instance Show UniqueId where
    show (UniqueId uid) = show uid

instance Enum UniqueId where
    fromEnum (UniqueId uid) = uid
    toEnum = UniqueId

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

data TaskUpdate = TaskError Int String String
                | TaskSuccess String
                deriving (Show, Eq)

data Task = PendingTask [TaskUpdate]
          | FailedTask { ctaskUpdates :: [TaskUpdate]
                       , ctaskTime    :: UTCTime
                       }
          | CompletedTask { ctaskUpdates  :: [TaskUpdate]
                          , ctaskBucketCF :: (Bucket, Maybe Text)
                          -- ^ The bucket this task belonged to, along with
                          -- the cloudfront distribution id (if it exists).
                          , ctaskTime     :: UTCTime
                          }
          deriving (Show, Eq)

completeTask :: (Bucket, Maybe Text) -> UTCTime -> Task -> Task
completeTask bcf utc (PendingTask tus) = CompletedTask tus bcf utc
completeTask _ _ t = t

failTask :: UTCTime -> Task -> Task
failTask utc (PendingTask tus) = FailedTask tus utc
failTask _ t = t

type Tasks = M.Map UniqueId Task
type TasksVar = TVar Tasks

data OpUploadTarball = OpUploadTarball { optbManager :: Manager
                                       , optbTmp :: FilePath
                                       , optbUID :: UniqueId
                                       , optbTaskVar :: TasksVar
                                       , optbCreds :: AwsCreds
                                       , optbBucketCF :: (Bucket, Maybe Text)
                                       -- ^ The bucket this op applied to, along with
                                       -- the cloudfront distribution id (if it exists).
                                       , optbAcl :: CannedAcl
                                       , optbKey :: Text
                                       , optbFile :: File
                                       }

data LogEntry = LogEntry { logUser   :: UserName
                         , logTime   :: UTCTime
                         , logParams :: [Param]
                         , logPath   :: Text
                         } deriving (Show, Read)
newtype Log = Log [LogEntry]
type LogVar = TVar Log

type CFDistros = M.Map Bucket Text
type CFDistrosVar = TVar CFDistros

data SaveState = SaveState { users :: Users
                           , distros :: CFDistros
                           } deriving (Show, Read, Eq)

data Pusher = Pusher { pLogVar    :: LogVar
                     , pUsersVar  :: UsersVar
                     , pCFDistros :: CFDistrosVar
                     , pConfig    :: Config
                     , pNextID    :: UIDVar
                     , pTasks     :: TasksVar
                     , pMngr      :: Manager
                     }
type ActionP = ActionT LT.Text (ReaderT Pusher IO)
type ScottyP = ScottyT LT.Text (ReaderT Pusher IO)
