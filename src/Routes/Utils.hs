{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Routes.Utils where

import Prelude as P
import Html
import Types
import UserOps
import Session
import Aws.S3.Core
import System.FilePath
import System.Directory (getCurrentDirectory, getTemporaryDirectory)
import Web.Scotty.Trans hiding (get, post)
import Network.HTTP.Types.Status
import Network.HTTP.Client
import Network.Wai
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString,IsString)
import Data.Time.Clock
import Data.Configurator as DC
import Data.Text as T
import Data.Text.IO (writeFile)
--import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty.Trans as WST
import qualified Control.Monad.Morph as MT
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Route helpers
--------------------------------------------------------------------------------
newUniqueId :: ActionP UniqueId
newUniqueId = do
    uidVar <- MT.lift $ asks pNextID
    uid <- liftIO $ atomically $ readTVar uidVar
    liftIO $ atomically $ modifyTVar' uidVar succ
    return uid

getTasksVar :: ActionP TasksVar
getTasksVar = MT.lift $ asks pTasks

getManager :: ActionP Manager
getManager = MT.lift $ asks pMngr

getUsers :: ActionP Users
getUsers = do
    uvar <- MT.lift $ asks pUsersVar
    liftIO $ atomically $ readTVar uvar

getCredsFor :: Text -> B.ByteString -> Bucket -> ActionP (Maybe AwsCreds)
getCredsFor name pass bucket = do
    users <- MT.lift $ asks pUsersVar
    liftIO $ atomically $ (getAwsCreds name pass bucket) <$> readTVar users

findUser :: UserName -> ActionP (Maybe UserDetail)
findUser name = do
    muser <- getUser
    users <- getUsers
    let op = do thatUser <- M.lookup name users
                thisUser <- muser
                if userLevel thatUser > userLevel thisUser || userName thatUser == name
                then return thatUser
                else fail ""
    return op

flushLogToDisk :: ActionP ()
flushLogToDisk = do
    utc  <- liftIO getCurrentTime
    cwd  <- liftIO getCurrentDirectory
    var  <- MT.lift $ asks pLogVar
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

saveDataToDisk :: ActionP ()
saveDataToDisk = do
    cfg       <- MT.lift $ asks pConfig
    saveFile  <- liftIO $ DC.lookupDefault "save.txt" cfg "save-file"
    users     <- getUsers
    cfdsVar   <- MT.lift $ asks pCFDistros
    cfds      <- liftIO $ atomically $ readTVar cfdsVar

    let save = SaveState users cfds

    liftIO $ do Data.Text.IO.writeFile saveFile $ T.pack $ show save

log_ :: Text -> ActionP ()
log_ path = do
    utc <- liftIO getCurrentTime
    ps  <- params
    musr <- getUser
    let ps' = P.map nullpass ps
        nullpass (k,v) = if k `elem` hides then (k,"***") else (k, v)
        hides = ["pass","authpass","key","secret"]
        usr = maybe ("guest") userName musr
        entry = LogEntry usr utc ps' path
    var <- MT.lift $ asks pLogVar
    Log lg <- liftIO $ atomically $ readTVar var
    when (P.length lg >= 1000) flushLogToDisk
    liftIO $ atomically $ modifyTVar' var $ \(Log lg') -> Log (entry:lg')

get :: Url -> ActionP () -> ScottyP ()
get url f = WST.get (fromString $ show url) $ log_ (T.pack $ show url) >> f

post :: Url -> ActionP () -> ScottyP ()
post url f = WST.post (fromString $ show url) $ log_ (T.pack $ show url) >> f

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

redirectToLogin :: ActionP ()
redirectToLogin = do
    path <- T.unpack . T.append "/" . T.intercalate "/" . pathInfo <$> request
    redirect $ LT.pack $ show $ Uri UrlUserLogin [("r", path)]

withAuthdNamePass :: UserName -> B.ByteString -> (UserDetail -> ActionP ()) -> ActionP ()
withAuthdNamePass authname authpass f = do
    users <- getUsers
    let authd = getUserIsValid authname authpass users
    if not authd
    then redirectToLogin
    else case M.lookup authname users of
             Nothing -> redirectToLogin
             Just u  -> writeLoginCookie u >> f u

withAuthdUser :: (UserDetail -> ActionP ()) -> ActionP ()
withAuthdUser f = do
    mu <- getUser
    case mu of
        Nothing -> redirectToLogin
        Just u  -> writeLoginCookie u >> f u

withAuthLvl :: Int -> ActionP () -> ActionP ()
withAuthLvl lvl f = withAuthdUser $ \UserDetail{..} -> do
    if userLevel <= lvl then f
      else do status unauthorized401
              blaze $ guestContainer $ H.div "unauthorized 401"



getUser :: ActionP (Maybe UserDetail)
getUser = do
    mcook <- readUserCookie
    users <- getUsers

    case mcook of
        Just (UserCookie UserDetail{..} _) -> return $ M.lookup userName users

        Nothing -> do mname <- optionalParam "authname"
                      mpass <- optionalParam "authpass"
                      let mnp = (,) <$> mname <*> mpass
                      case mnp of
                          Nothing -> return Nothing
                          Just (n,p) -> return $ if getUserIsValid n p users
                                                 then M.lookup n users
                                                 else fail ""

saveUser :: UserDetail -> ActionP (Either String ())
saveUser u@UserDetail{..} = do
    uv <- MT.lift $ asks pUsersVar
    us <- liftIO $ atomically $ readTVar uv
    case M.lookup userName us of
        Nothing -> return $ Left $ "Could not find user " ++ T.unpack userName
        Just _  -> do liftIO $ atomically $ modifyTVar' uv $ M.insert userName u
                      saveDataToDisk
                      return $ Right ()

getCloudfrontDistroForBucket :: Bucket -> ActionP (Maybe Text)
getCloudfrontDistroForBucket b = do
    cfdsVar <- MT.lift $ asks pCFDistros
    cfds <- liftIO $ atomically $ readTVar cfdsVar
    case M.lookup b cfds of
        Nothing -> return Nothing
        Just "" -> return Nothing
        Just a  -> return $ Just a

getHtmlContainer :: ActionP (H.Html -> H.Html)
getHtmlContainer = do
    mu <- getUser
    return $ maybe guestContainer (const userContainer) mu

getDefaultCreds :: ActionP (Maybe AwsCreds)
getDefaultCreds = do
    muser <- getUser
    case muser of
        Nothing -> return Nothing
        Just (UserDetail{..}) -> do bucket <- param "bucket"
                                    getCredsFor userName userPass bucket

withDefaultCreds :: (AwsCreds -> ActionP ()) -> ActionP ()
withDefaultCreds f = do
    mcreds  <- getDefaultCreds
    case mcreds of
        Nothing -> redirectToLogin
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

getTempDir :: ActionP FilePath
getTempDir = do
    cfg <- MT.lift $ asks pConfig
    tmp <- liftIO $ do tmp <- getTemporaryDirectory
                       lookupDefault tmp cfg "tmp-dir"
    return tmp

showS3Error :: S3Error -> ActionP ()
showS3Error err = do
    status status500
    blaze $ userContainer $ H.toHtml err
