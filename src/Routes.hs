{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Routes where

import Prelude as P
import Session
import Routes.Utils
import Types
import Html
import UserOps
import AwsOps
import Aws.S3.Core
import System.Directory (doesFileExist)
import Web.Scotty.Trans hiding (get, post)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Mime
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.Text as T
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty.Trans as WST
import qualified Control.Monad.Morph as MT
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M

routes :: ScottyP ()
routes = do
    get "/login" $ do
        r <- defaultParam "r" "/"
        blaze $ loginHtml r
    post "/login" $ do
        name <- param "authname"
        pass <- param "authpass"
        withAuthdNamePass name pass $ \u -> do
            writeLoginCookie u
            r <- defaultParam "r" "/"
            redirect r

    get "/logout" $ do
        expireLoginCookie
        blaze logoutHtml

    -- Misc
    WST.get "/log" $ withAuthdUser $ const $ do
        logVar  <- MT.lift $ asks pLogVar
        (Log lg) <- liftIO $ atomically $ readTVar logVar
        blaze $ logHtml lg

    get "/" $ getHtmlContainer >>= \c -> blaze $ c "hello"

    get "/auth-check-lvl" $ do
        lvl <- param "lvl"
        withAuthLvl lvl $ blaze $ userContainer "okay"

    get "/auth-check-bucket" $ do
        withDefaultCreds $ const $ blaze $ userContainer "okay"

    -- Users
    get "/users" $ withAuthdUser $ const $ do
        usersVar <- MT.lift $ asks pUsersVar
        users' <- liftIO $ atomically $ readTVar usersVar
        blaze $ usersHtml users'

    get "/users.txt" $ do
        withAuthLvl 0 $ do
            usersVar <- MT.lift $ asks pUsersVar
            (users' :: Users) <- liftIO $ atomically $ readTVar usersVar
            text $ LT.pack $ show users'

    get "/user" $ withAuthdUser $ const $ blaze newUserHtml
    post "/user" postUserRoute

    get "/user/password" $ withAuthdUser $ const $ blaze passwordHtml
    post "/user/password" $ withAuthdUser $ \u -> do
        pass <- param "pass"
        chk  <- param "passCheck"
        if pass /= chk
        then blaze $ userContainer "Those passwords don't match :("
        else do u' <- liftIO $ updatePassword u pass
                e  <- saveUser u'
                case e of
                    Left err -> blaze $ userContainer $ H.toHtml err
                    Right _  -> blaze $ userContainer "Password updated."

    get "/user-add-bucket" $ withAuthdUser $ \UserDetail{..} ->
        blaze $ userAddBucketHtml userName
    post "/user-add-bucket" $ withAuthdUser $ const $ do
        username <- param "username"
        bucket   <- param "bucket" :: ActionP Bucket
        key      <- param "key"
        secret   <- param "secret"
        muser    <- findUser username
        uVar     <- MT.lift $ asks pUsersVar

        case muser of
            Nothing -> blaze $ userContainer "It seems you can't update that user."
            Just u  -> do let creds = M.fromList [(bucket, AwsCreds key secret)]
                              u' = addCreds u creds
                          liftIO $ atomically $
                              modifyTVar' uVar $ M.insert username u'
                          flushUsersToDisk
                          blaze $ userContainer "Okay, credentials added."

    get "/user/settings" $ withAuthdUser $ \u ->
        blaze $ userSettingsHtml u

    -- Querying
    get "/list" $ withAuthdUser $ \u -> blaze $ listBucketFormHtml $ userBuckets u
    post "/list" $ withAuthdUser $ \(UserDetail{..}) -> do
        bucket  <- param "bucket"
        mprefix <- nothingIfNull <$> optionalParam "prefix"
        mdelim  <- nothingIfNull <$> optionalParam "delimiter"
        let mcreds = M.lookup bucket userCreds
        case mcreds of
            Nothing -> blaze $ userContainer "Seems you don't have this bucket's credentials."
            Just c  -> do infos <- liftIO $ listDirectory c bucket mprefix mdelim
                          blaze $ listBucketHtml $ P.map objectKey infos

    get "/task/:task" $ withAuthdUser $ const $ do
        task  <- param "task"
        tvar  <- MT.lift $ asks pTasks
        tasks <- liftIO $ atomically $ readTVar tvar
        case M.lookup (UniqueID task) tasks of
            Nothing -> blaze $ userContainer "It seems there is no such task."
            Just s  -> blaze $ taskHtml s

    -- Uploading new files
    get "/upload" $ withAuthdUser $ \u -> blaze $ uploadHtml $ userBuckets u
    post "/upload" postUploadRoute

    -- Uploading a zip of an entire directory
    get "/upload-zip" $ withAuthdUser $ \u ->
        blaze $ uploadZipHtml $ userBuckets u
    post "/upload-zip" postUploadZipRoute

    -- Copying existing files
    get "/copy" $ withAuthdUser $ \u -> blaze $ copyHtml $ userBuckets u
    post "/copy" $ withAuthdUser $ \(UserDetail{..}) -> do
        fbucket   <- param "bucket"
        toBucket <- optionalParam "toBucket"
        from     <- param "from"
        to       <- param "to"
        let tbucket = maybe fbucket id toBucket
            mc = do cf <- M.lookup fbucket userCreds
                    ct <- M.lookup tbucket userCreds
                    return (FileAccess cf fbucket from, FileAccess ct tbucket to)
        case mc of
            Nothing -> blaze $ userContainer "Seems you don't have a bucket's credentials."
            Just c  -> do _ <- liftIO $ uncurry copyFile c
                          blaze $ userContainer "okay"

    get "/copy-folder" $ withAuthdUser $ \u -> blaze $ copyFolderHtml $ userBuckets u
    post "/copy-folder" $ withAuthdUser $ \(UserDetail{..}) -> do
        fbucket  <- param "bucket"
        toBucket <- optionalParam "toBucket"
        from     <- param "from"
        to       <- param "to"
        let tbucket = maybe fbucket id toBucket
            mc = do cf <- M.lookup fbucket userCreds
                    ct <- M.lookup tbucket userCreds
                    return (FileAccess cf fbucket from, FileAccess ct tbucket to)
        liftIO $ print mc
        case mc of
            Nothing -> blaze $ userContainer "Seems you don't have a bucket's credentials."
            Just c  -> do _ <- liftIO $ uncurry copyFile c
                          blaze $ userContainer "okay"

    -- Catchall with static dir lookup
    WST.get (function $ const $ Just []) $ do
        path <- B.unpack . rawPathInfo <$> request
        withStaticDir $ \dir -> do
            let fp = dir ++ path
                mt = defaultMimeLookup $ T.pack fp
            exists <- liftIO $ doesFileExist fp
            if not exists
            then do status notFound404
                    blaze $ guestContainer $ H.toHtml $ "'" ++ path ++ "' does not exist."
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
              blaze $ userContainer "unauthorized 401"

postUploadRoute :: ActionP ()
postUploadRoute = withAuthdUser $ \UserDetail{userCreds=creds} -> do
    buck  <- param "bucket"
    ctype <- optionalParam "content-type"
    cenc  <- optionalParam "content-encoding"
    mkey  <- optionalParam "key"
    acl   <- defaultParam "acl" AclPublicRead
    mngr  <- MT.lift $ asks pMngr
    case M.lookup buck creds of
        Nothing -> blaze $ userContainer "no creds for the bucket"
        Just c  -> do fs <- files
                      ets <- forM fs (liftIO . uploadFile mngr c buck ctype cenc acl mkey)
                      blaze $ userContainer $ H.div $ forM_ ets $ \et -> do
                                   case et of
                                       Left err -> H.p $ H.toHtml $ show err
                                       Right po -> H.p $ H.toHtml $ show po

postUploadZipRoute :: ActionP ()
postUploadZipRoute = withAuthdUser $ \UserDetail{userCreds=creds} -> do
    buck   <- param "bucket"
    key    <- param "key"
    acl    <- defaultParam "acl" AclPublicRead
    uid    <- newUniqueID
    case M.lookup buck creds of
        Nothing -> blaze $ userContainer "no creds for the bucket"
        Just c -> do f    <- P.head <$> files
                     tmp  <- getTempDir
                     tvar <- MT.lift $ asks pTasks
                     mngr <- MT.lift $ asks pMngr
                     liftIO $ uploadZippedDir mngr tmp uid tvar c buck acl key f
                     blaze $ taskLinkHtml uid

