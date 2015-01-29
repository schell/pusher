{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Routes where

import Prelude as P
import Types
import Html
import Page.Upload
import Page.NewUser
import UserOps
import AwsOps
import Aws.S3.Core
import System.FilePath
import System.Directory (doesFileExist, getCurrentDirectory)
import Web.Scotty.Trans hiding (get, post)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Mime
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (forM,when)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString,IsString)
import Data.Time.Clock
import Data.Configurator as DC
import Data.Text as T
import Data.Text.IO (writeFile)
import qualified Web.Scotty.Trans as WST
import qualified Control.Monad.Morph as MT
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Route helpers
--------------------------------------------------------------------------------
flushLogToDisk :: ActionP ()
flushLogToDisk = do
    utc <- liftIO getCurrentTime
    cwd <- liftIO getCurrentDirectory
    var <- MT.lift $ asks pLogVar
    -- Get our log file.
    cfg <- MT.lift $ asks pConfig
    logFile <- liftIO (DC.lookupDefault "log" cfg "log-file" :: IO String)
    let lfp = if P.head logFile == '/'
                then logFile
                else cwd </> logFile

    Log lg <- liftIO $ atomically $ readTVar var
    let fn = P.unwords [lfp, "-", show utc]
    liftIO $ do Data.Text.IO.writeFile fn $ T.pack $ show lg
                atomically $ modifyTVar' var (const $ Log [])

flushUsersToDisk :: ActionP ()
flushUsersToDisk = do
    cfg       <- MT.lift $ asks pConfig
    usersFile <- liftIO $ DC.lookupDefault "users.txt" cfg "users-file"
    usersVar  <- MT.lift $ asks pUsersVar
    users     <- liftIO $ atomically $ readTVar usersVar
    liftIO $ do Data.Text.IO.writeFile usersFile $ T.pack $ show users

log_ :: Text -> ActionP ()
log_ path = do
    utc <- liftIO getCurrentTime
    ps  <- params
    let ps' = P.map nullpass ps
        nullpass ("pass",_) = ("pass", "")
        nullpass p = p
        entry = LogEntry utc ps' path
    var <- MT.lift $ asks pLogVar
    Log lg <- liftIO $ atomically $ readTVar var
    when (P.length lg >= 1000) flushLogToDisk
    liftIO $ atomically $ modifyTVar' var $ \(Log lg') -> Log (entry:lg')

get :: String -> ActionP () -> ScottyP ()
get str f = WST.get (fromString str) $ log_ (T.pack str) >> f

post :: String -> ActionP () -> ScottyP ()
post str f = WST.post (fromString str) $ log_ (T.pack str) >> f

optionalParam :: Parsable a
              => LT.Text -> ActionP (Maybe a)
optionalParam x = (fmap Just $ param x) `rescue` (const $ return Nothing)

defaultParam :: Parsable a
             => LT.Text -> a -> ActionP a
defaultParam x def = do
    mP <- optionalParam x
    return $ case mP of
        Nothing -> def
        Just pm -> pm

withAuthLvl :: Int -> ActionP () -> ActionP ()
withAuthLvl lvl f = do
    authname <- param "authname"
    authpass <- param "authpass"
    usersVar <- MT.lift $ asks pUsersVar
    users'   <- liftIO $ atomically $ readTVar usersVar
    let authd   = getUserIsValid authname authpass users'
        authlvl = maybe 1000 id $ getUserLevel authname users'

    if authd && authlvl <= lvl then f
      else do status unauthorized401
              html "unauthorized 401"

getCredsFor :: Text -> B.ByteString -> Bucket -> ActionP (Maybe AwsCreds)
getCredsFor name pass bucket = do
    users <- MT.lift $ asks pUsersVar
    liftIO $ atomically $ (getAwsCreds name pass bucket) <$> readTVar users

getDefaultCreds :: ActionP (Maybe AwsCreds)
getDefaultCreds = do
    name <- param "name"
    pass <- param "pass"
    bucket <- param "bucket"
    getCredsFor name pass bucket

withDefaultCreds :: (AwsCreds -> ActionP ()) -> ActionP ()
withDefaultCreds f = do
    mcreds  <- getDefaultCreds
    case mcreds of
        Nothing -> do status unauthorized401
                      html "unauthorized 401"
        Just c  -> f c

withCredsFor :: Text -> B.ByteString -> Bucket -> (AwsCreds -> ActionP ())
             -> ActionP ()
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

nothingIfNull :: (IsString a, Eq a)
              => Maybe a -> Maybe a
nothingIfNull m = case m of
                      Nothing -> Nothing
                      Just "" -> Nothing
                      Just b  -> Just b

withStaticDir :: (String -> ActionP a) -> ActionP a
withStaticDir f = do
    cfg <- MT.lift $ asks pConfig
    dir <- liftIO $ do cwd <- getCurrentDirectory
                       lookupDefault (cwd ++ "/static") cfg "static-dir"
    f dir

--------------------------------------------------------------------------------
-- The actual routes
--------------------------------------------------------------------------------
routes :: ScottyP ()
routes = do
    -- Misc
    WST.get "/log" $ do
        logVar  <- MT.lift $ asks pLogVar
        (Log lg) <- liftIO $ atomically $ readTVar logVar
        text $ LT.pack $ show lg

    get "/" $ html $ "hello"
    get "/auth-check-lvl" $ do
        lvl <- param "lvl"
        withAuthLvl lvl $ text "okay"

    get "/auth-check-bucket" $ do
        withDefaultCreds $ const $ text "okay"

    -- Users
    get "/users" $ do
        usersVar <- MT.lift $ asks pUsersVar
        users' <- liftIO $ atomically $ readTVar usersVar
        text $ LT.fromStrict $ quantifyUsers $ M.toList users'

    get "/users.txt" $ do
        withAuthLvl 0 $ do
            usersVar <- MT.lift $ asks pUsersVar
            (users' :: Users) <- liftIO $ atomically $ readTVar usersVar
            text $ LT.pack $ show users'

    get "/user" $ blaze newUserPage
    post "/user" postUserRoute

    -- Querying
    get "/list" $ do
        bucket  <- param "bucket"
        mprefix <- nothingIfNull <$> optionalParam "prefix"
        mdelim  <- nothingIfNull <$> optionalParam "delimiter"
        withDefaultCreds $ \c -> do
            infos <- liftIO $ listDirectory c bucket mprefix mdelim
            text $ LT.intercalate "\n" $ P.map (LT.fromStrict . objectKey) infos

    -- Uploading new files
    get "/upload" $ blaze uploadPage
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
            Just c  -> do _ <- liftIO $ uncurry copyFile c
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
            Just c  -> do _ <- liftIO $ uncurry copyDirectory c
                          text "okay"

    -- Catchall with static dir lookup
    WST.get (function $ const $ Just []) $ do
        path <- B.unpack . rawPathInfo <$> request
        withStaticDir $ \dir -> do
            let fp = dir ++ path
                mt = defaultMimeLookup $ T.pack fp
            exists <- liftIO $ doesFileExist fp
            if not exists
            then do status notFound404
                    text $ LT.pack $ "'" ++ path ++ "' does not exist."
            else do bytes <- liftIO $ B.readFile fp
                    addHeader "Content-Type" $ LT.pack $ B.unpack mt
                    raw $ LB.fromStrict bytes

postUserRoute :: ActionP ()
postUserRoute = do
    name     <- param "name"
    pass     <- param "pass"
    addlvl   <- param "lvl"
    usersVar <- MT.lift $ asks pUsersVar
    users'   <- liftIO $ atomically $ readTVar usersVar
    let userlvl = maybe 10 id $ getUserLevel name users'
    if userlvl >= addlvl
      then withAuthLvl userlvl $ do
               mbuck <- optionalParam "bucket"
               mkey  <- optionalParam "key"
               msecr <- optionalParam "secret"

               msg  <- MT.lift $ asks pUsersVar >>= \users -> liftIO $
                   addUser users name addlvl pass (nothingIfNull mbuck) $ do
                       key  <- nothingIfNull mkey
                       secr <- nothingIfNull msecr
                       return $ AwsCreds key secr
               flushUsersToDisk
               html $ LT.fromStrict msg
      else do status unauthorized401
              html "unauthorized 401"

postUploadRoute :: ActionP ()
postUploadRoute = do
    name  <- param "name"
    pass  <- param "pass"
    buck  <- param "bucket"
    ctype <- optionalParam "content-type"
    cenc  <- optionalParam "content-encoding"
    mkey  <- optionalParam "key"
    acl   <- defaultParam "acl" AclPublicRead
    users <- MT.lift $ asks pUsersVar

    mCreds <- liftIO $ atomically $ (getAwsCreds name pass buck) <$> readTVar users

    case mCreds of
        Nothing -> html "invalid pass"
        Just c  -> do fs <- files
                      ts <- forM fs (liftIO . uploadFile c buck ctype cenc acl mkey)
                      html $ LT.fromStrict $ T.concat ts
