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
import Aws.S3
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
import Data.String (fromString)
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
    get UrlUserLogin $ do
        r <- defaultParam "r" "/"
        blaze $ loginHtml r
    post UrlUserLogin $ do
        name <- param "authname"
        pass <- param "authpass"
        withAuthdNamePass name pass $ \u -> do
            writeLoginCookie u
            r <- defaultParam "r" "/"
            redirect r

    get UrlUserLogout $ do
        expireLoginCookie
        blaze logoutHtml

    -- Misc
    WST.get (fromString $ show UrlLogView) $ withAuthdUser $ const $ do
        logVar  <- MT.lift $ asks pLogVar
        (Log lg) <- liftIO $ atomically $ readTVar logVar
        blaze $ logHtml lg

    get UrlHome $ getHtmlContainer >>= \c -> blaze $ c "hello"

    -- Users
    get UrlUsers $ withAuthdUser $ const $ do
        usersVar <- MT.lift $ asks pUsersVar
        users' <- liftIO $ atomically $ readTVar usersVar
        blaze $ usersHtml users'

    get UrlUsersFile $ do
        withAuthLvl 0 $ do
            usersVar <- MT.lift $ asks pUsersVar
            (users' :: Users) <- liftIO $ atomically $ readTVar usersVar
            text $ LT.pack $ show users'

    get UrlUserAdd $ withAuthdUser $ const $ blaze newUserHtml
    post UrlUserAdd postUserRoute

    get UrlUserPassword $ withAuthdUser $ const $ blaze passwordHtml
    post UrlUserPassword $ withAuthdUser $ \u -> do
        pass <- param "pass"
        chk  <- param "passCheck"
        if pass /= chk
        then blaze $ userContainer "Those passwords don't match :("
        else do u' <- liftIO $ updatePassword u pass
                e  <- saveUser u'
                case e of
                    Left err -> blaze $ userContainer $ H.toHtml err
                    Right _  -> blaze $ userContainer "Password updated."

    get UrlUserSettings $ withAuthdUser $ \u ->
        blaze $ userSettingsHtml u

    -- Buckets
    get UrlBucketAdd $ withAuthdUser $ \UserDetail{..} ->
        blaze $ userAddBucketHtml userName
    post UrlBucketAdd $ withAuthdUser $ const $ do
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
                          saveDataToDisk
                          blaze $ userContainer "Okay, credentials added."

    get UrlBucketLinkCF $ withAuthdUser $ \UserDetail{..} -> do
        cfdsVar <- MT.lift $ asks pCFDistros
        cfds    <- liftIO $ atomically $ readTVar cfdsVar
        let creds  = "" <$ userCreds
            cfds' = M.union cfds creds
        blaze $ userContainer $ cloudfrontHtml cfds'
    post UrlBucketLinkCF $ withAuthdUser $ const $ do
        ps      <- params
        cfdsVar <- MT.lift $ asks pCFDistros
        cfds    <- liftIO $ atomically $ readTVar cfdsVar
        let cfds' = LT.toStrict <$> (M.mapKeys LT.toStrict $ M.fromList ps)
            cmp a b = if a == b then Nothing else Just $ T.unwords [a,"->",b]
            diff = M.differenceWith cmp cfds cfds'
        liftIO $ atomically $ modifyTVar' cfdsVar $ const $ cfds' `M.union` cfds
        saveDataToDisk
        blaze $ userContainer $ do
            H.p "Updated cloudfront associations."
            H.pre $ H.toHtml $ show diff

    get UrlBucketList $ withAuthdUser $ \u -> blaze $ listBucketFormHtml $ userBuckets u
    post UrlBucketList $ withAuthdUser $ \(UserDetail{..}) -> do
        bucket  <- param "bucket"
        mprefix <- nothingIfNull <$> optionalParam "prefix"
        mdelim  <- nothingIfNull <$> optionalParam "delimiter"
        let mcreds = M.lookup bucket userCreds
        case mcreds of
            Nothing -> blaze $ userContainer "Seems you don't have this bucket's credentials."
            Just c  -> do
                egbr <- listDirectory c bucket mprefix mdelim
                case egbr of
                    Left err -> showS3Error err
                    Right GetBucketResponse{..} ->
                        blaze $ listBucketHtml $ P.map objectKey gbrContents

    get UrlTask $ withAuthdUser $ const $ do
        task  <- param "task"
        tvar  <- MT.lift $ asks pTasks
        tasks <- liftIO $ atomically $ readTVar tvar
        case M.lookup (UniqueId task) tasks of
            Nothing -> blaze $ userContainer "It seems there is no such task."
            Just s  -> blaze $ taskHtml s

    -- Uploading new files
    get UrlUploadFile $ withAuthdUser $ \u -> blaze $ uploadHtml $ userBuckets u
    post UrlUploadFile postUploadRoute

    -- Uploading a zip of an entire directory
    get UrlUploadTarball $ withAuthdUser $ \u ->
        blaze $ uploadZipHtml $ userBuckets u
    post UrlUploadTarball postUploadZipRoute

    -- Copying existing files
    get UrlCopyFile $ withAuthdUser $ \u -> blaze $ copyHtml $ userBuckets u
    post UrlCopyFile $ withAuthdUser $ \(UserDetail{..}) -> do
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
            Just fs -> do
                if fbucket == tbucket
                then inBucket
                else acrossBuckets
                where inBucket = do ecor <- copyFileInBucket (fst fs) to
                                    case ecor of
                                        Left err -> showS3Error err
                                        Right _ -> blaze $ userContainer "okay"
                      acrossBuckets = do epor <- uncurry copyFile fs
                                         case epor of
                                             Left err -> showS3Error err
                                             Right _ -> blaze $ userContainer "okay"

    get UrlCopyFolder $ withAuthdUser $ \u -> blaze $ copyFolderHtml $ userBuckets u
    post UrlCopyFolder $ withAuthdUser $ \(UserDetail{..}) -> do
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
            Just c  -> do
                if fbucket == tbucket
                then inBucket
                else acrossBuckets
                where inBucket = do ecors <- copyDirectoryInBucket (fst c) to
                                    case ecors of
                                        Left err -> showS3Error err
                                        Right _  -> blaze $ userContainer "okay"
                      acrossBuckets = do epors <- uncurry copyDirectory c
                                         case epors of
                                             Left err -> showS3Error err
                                             Right _  -> blaze $ userContainer "okay"

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
               saveDataToDisk
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
    case M.lookup buck creds of
        Nothing -> blaze $ userContainer "no creds for the bucket"
        Just c  -> do fs <- files
                      ts <- forM fs (uploadFile c buck ctype cenc acl mkey)
                      blaze $ userContainer $ H.div $ forM_ ts $ \uid ->
                          taskLinkHtml uid

postUploadZipRoute :: ActionP ()
postUploadZipRoute = withAuthdUser $ \UserDetail{userCreds=creds} -> do
    buck   <- param "bucket"
    key    <- param "key"
    acl    <- defaultParam "acl" AclPublicRead
    case M.lookup buck creds of
        Nothing -> blaze $ userContainer "no creds for the bucket"
        Just c -> do f    <- P.head <$> files
                     mcf  <- getCloudfrontDistroForBucket buck
                     (uid, fs) <- uploadZippedDir c buck acl key f
                     blaze $ userContainer $ do
                         taskLinkHtml uid
                         flip (maybe (return ())) mcf $ \cf -> do
                             cfUrl cf
                             H.pre $ H.toHtml $ intercalate "\n" $ P.map pack fs

