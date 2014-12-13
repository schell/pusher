{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import UserOps
import AwsOps
import Aws.S3.Core
import System.Environment
import System.IO
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath
import Web.Scotty.Trans hiding (get, post)
import qualified Web.Scotty.Trans as WST
import Network.HTTP.Types.Status
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Reader
import Control.Monad
import Data.String (fromString)
import Data.Time.Clock
import Data.Typeable
import Data.Configurator.Types
import Data.Configurator as DC
import Data.Text as T
import Data.Text.IO (readFile, appendFile)
import qualified Control.Monad.Morph as MT
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M

deriving instance Typeable Config
type ActionP = ActionT LT.Text (ReaderT Pusher IO)
type ScottyE = ScottyT LT.Text (ReaderT Pusher IO)

data Pusher = Pusher { pLogVar   :: LogVar
                     , pUsersVar :: UsersVar
                     , pConfig   :: Config
                     }

data LogEntry = LogEntry { logTime   :: UTCTime
                         , logParams :: [Param]
                         , logPath   :: Text
                         } deriving (Show, Read)

newtype Log = Log [LogEntry] deriving (Typeable)
type LogVar = TVar Log

modifyVar f = ask >>= modifyTVar' f

log_ path = do
    utc <- liftIO getCurrentTime
    ps  <- params
    let entry = LogEntry utc ps path
    var <- asks pLogVar
    liftIO $ atomically $ modifyTVar' var $ \(Log lg) -> Log (entry:lg)

get str f = WST.get (fromString str) $ log_ (T.pack str) >> f

post str f = WST.post (fromString str) $ log_ (T.pack str) >> f

optionalParam :: Parsable a
              => LT.Text -> ActionP (Maybe a)
optionalParam x = (fmap Just $ param x) `rescue` (const $ return Nothing)

--optionalText :: LT.Text -> ActionE r (Maybe LT.Text)
--optionalText

defaultParam :: Parsable a
             => LT.Text -> a -> ActionP a
defaultParam x def = do
    mP <- optionalParam x
    return $ case mP of
        Nothing -> def
        Just pm -> pm

withAuthLvl lvl f = do
    authname <- param "authname"
    authpass <- param "authpass"
    users'   <- liftIO . atomically . readTVar =<< asks pUsersVar
    let authd   = getUserIsValid authname authpass users'
        authlvl = maybe 1000 id $ getUserLevel authname users'

    if authd && authlvl <= lvl then f
      else do status unauthorized401
              html "unauthorized 401"

getCredsFor name pass bucket = do
    users <- asks pUsersVar
    liftIO $ atomically $ (getAwsCreds name pass bucket) <$> readTVar users

getDefaultCreds = do
    name <- param "name"
    pass <- param "pass"
    bucket <- param "bucket"
    getCredsFor name pass bucket

withDefaultCreds f = do
    mcreds  <- getDefaultCreds
    case mcreds of
        Nothing -> do status unauthorized401
                      html "unauthorized 401"
        Just c  -> f c

withCredsFor name pass bucket f = do
    mc <- getCredsFor name pass bucket
    case mc of
        Nothing -> return ()
        Just c  -> f c

instance Parsable CannedAcl where
   parseParam "AclPrivate" = Right AclPrivate
   parseParam "AclPublicRead" = Right AclPublicRead
   parseParam "AclPublicReadWrite" = Right AclPublicReadWrite
   parseParam "AclAuthenticatedRead" = Right AclAuthenticatedRead
   parseParam "AclBucketOwnerRead" = Right AclBucketOwnerRead
   parseParam "AclBucketOwnerFullControl" = Right AclBucketOwnerFullControl
   parseParam "AclLogDeliveryWrite" = Right AclLogDeliveryWrite
   parseParam _ = Left "Not a valid canned ACL"

main :: IO ()
main = do
    -- Get the config file from the command line.
    args <- getArgs
    cfg <- case args of
        []  -> return $ DC.empty
        f:_ -> fst <$> autoReload autoConfig [Required f]

    cwd <- getCurrentDirectory

    -- Get our configuration.
    port  <- lookupDefault 3000 cfg "port"
    -- Get our startup users file from the config.
    mUsersFile <- DC.lookup cfg "users-file"
    -- Get our log file.
    logFile <- DC.lookupDefault "log.txt" cfg "log-file" :: IO String
    let logPath = if Prelude.head logFile == '/'
                    then logFile
                    else cwd </> logFile
    -- Create our user map.
    users <- case mUsersFile of
        Nothing -> return M.empty
        Just f  -> do fe <- doesFileExist $ cwd </> f
                      if fe
                        then do s <- System.IO.readFile $ cwd </> f
                                return $ (read s :: Users)
                        else return M.empty

    -- Create our "persistent" user/deploy list.
    putStrLn "Starting up with super user: "
    print users
    usersVar <- atomically $ newTVar users
    logVar   <- atomically $ newTVar $ Log []

    let nothingIfNull m = case m of
                              Nothing -> Nothing
                              Just "" -> Nothing
                              Just b  -> Just b

    -- Start up our good old scotty and give him some routes.
    let r = flip runReaderT (Pusher logVar usersVar cfg)

    scottyT port r r $ do
        -- Misc
        WST.get "/log" $ do
            (Log lg) <- asks pLogVar >>= liftIO . atomically . readTVar
            text $ LT.pack $ show lg

        get "/" $ html $ "hello"
        get "/auth-check" $ do
            lvl <- param "lvl"
            withAuthLvl lvl $ text "okay"

        -- Users
        get "/users" $ do
            users' <- liftIO . atomically . readTVar =<< asks pUsersVar
            text $ LT.fromStrict $ quantifyUsers $ M.toList users'

        get "/users.txt" $ do
            withAuthLvl 0 $ do
                (users' :: Users) <- liftIO . atomically . readTVar =<< asks pUsersVar
                text $ LT.pack $ show users'

        get "/user" $ (liftIO $ Data.Text.IO.readFile "static/new-user.html")
            >>= html . LT.fromStrict

        post "/user" postUserRoute

        -- Querying
        get "/list" $ do
            bucket  <- param "bucket"
            mprefix <- nothingIfNull <$> optionalParam "prefix"
            mdelim  <- nothingIfNull <$> optionalParam "delimiter"
            withDefaultCreds $ \c -> do
                infos <- liftIO $ listDirectory c bucket mprefix mdelim
                text $ LT.intercalate "\n" $ Prelude.map (LT.fromStrict . objectKey) infos

        -- Uploading new files
        post "/upload" postUploadRoute

        -- Copying existing files
        get "/copy" $ do
            name     <- param "name"
            pass     <- param "pass"
            fbucket   <- param "bucket"
            toBucket <- optionalParam "toBucket"
            from     <- param "from"
            to       <- param "to"
            let tbucket = maybe fbucket id toBucket
            mcf <- getCredsFor name pass fbucket
            mct <- getCredsFor name pass tbucket
            let mc = do cf <- mcf
                        ct <- mct
                        return (FileAccess cf fbucket from, FileAccess ct tbucket to)
            case mc of
                Nothing -> status unauthorized401 >> text "unauthorized 401"
                Just c  -> do liftIO $ uncurry copyFile c
                              text "okay"

        get "/copy-folder" $ do
            name     <- param "name"
            pass     <- param "pass"
            fbucket  <- param "bucket"
            toBucket <- optionalParam "toBucket"
            from     <- param "from"
            to       <- param "to"
            let tbucket = maybe fbucket id toBucket
            mcf <- getCredsFor name pass fbucket
            mct <- getCredsFor name pass tbucket
            let mc = do cf <- mcf
                        ct <- mct
                        return (FileAccess cf fbucket from, FileAccess ct tbucket to)
            liftIO $ print mc
            case mc of
                Nothing -> status unauthorized401 >> text "unauthorize 401"
                Just c  -> do liftIO $ uncurry copyDirectory c
                              text "okay"

        -- Catchall
        notFound $ text "not found 404"

postUserRoute = do
    name     <- param "name"
    pass     <- param "pass"
    addlvl   <- param "lvl"
    users'   <- liftIO . atomically . readTVar =<< asks pUsersVar
    let userlvl = maybe 10 id $ getUserLevel name users'
    if userlvl >= addlvl
      then withAuthLvl userlvl $ do
               mbuck <- optionalParam "bucket"
               mkey  <- optionalParam "key"
               msecr <- optionalParam "secret"
               let nothingIfNull m = case m of
                                         Nothing -> Nothing
                                         Just "" -> Nothing
                                         Just b  -> Just b

               msg  <- asks pUsersVar >>= \users -> liftIO $
                   addUser users name addlvl pass (nothingIfNull mbuck) $ do
                       key  <- nothingIfNull mkey
                       secr <- nothingIfNull msecr
                       return $ AwsCreds key secr
               html $ LT.fromStrict msg
      else do status unauthorized401
              html "unauthorized 401"

postUploadRoute = do
    name  <- param "name"
    pass  <- param "pass"
    buck  <- param "bucket"
    (ctype :: Maybe B.ByteString) <- optionalParam "content-type"
    (cenc :: Maybe Text)  <- optionalParam "content-encoding"
    acl   <- defaultParam "acl" AclPublicRead
    users <- asks pUsersVar

    mCreds <- liftIO $ atomically $ (getAwsCreds name pass buck) <$> readTVar users

    case mCreds of
        Nothing -> html "invalid pass"
        Just c  -> do fs <- files
                      ts <- forM fs (liftIO . uploadFile c buck ctype cenc acl)
                      html $ LT.fromStrict $ T.concat ts
