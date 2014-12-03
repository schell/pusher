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
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Data.Typeable
import Data.Configurator.Types
import Data.Configurator as DC
import Data.Text as T
import Data.Text.IO (readFile)
import qualified Control.Monad.Morph as MT
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M

deriving instance Typeable Config
type ActionE r = ActionT LT.Text (Eff r)
type ScottyE r = ScottyT LT.Text (Eff r)


optionalParam :: Parsable a
              => LT.Text -> ActionE r (Maybe a)
optionalParam x = (fmap Just $ param x) `rescue` (const $ return Nothing)

--optionalText :: LT.Text -> ActionE r (Maybe LT.Text)
--optionalText

defaultParam :: Parsable a
             => LT.Text -> a -> ActionE r a
defaultParam x def = do
    mP <- optionalParam x
    return $ case mP of
        Nothing -> def
        Just pm -> pm

withAuthLvl :: ( SetMember Lift (Lift IO) r
               , Member (Reader UsersVar) r)
             => Int -> ActionE r () -> ActionE r ()
withAuthLvl lvl f = do
    authname <- param "authname"
    authpass <- param "authpass"
    users'   <- liftE . atomically . readTVar =<< askE
    let authd   = getUserIsValid authname authpass users'
        authlvl = maybe 1000 id $ getUserLevel authname users'

    if authd && authlvl <= lvl then f
      else do status unauthorized401
              html "unauthorized 401"

getCreds :: ( SetMember Lift (Lift IO) r
            , Member (Reader UsersVar) r) => ActionE r (Maybe AwsCreds)
getCreds = do
    name  <- param "name"
    pass  <- param "pass"
    buck  <- param "bucket"
    users <- askE
    liftE $ atomically $ (getAwsCreds name pass buck) <$> readTVar users

withCreds f = do
    mcreds  <- getCreds
    case mcreds of
        Nothing -> do status unauthorized401
                      html "unauthorized 401"
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

    -- Get our configuration.
    port  <- lookupDefault 3000 cfg "port"
    -- Get our startup users file from the config.
    mUsersFile <- DC.lookup cfg "users-file"
    -- Create our user map.
    users <- case mUsersFile of
        Nothing -> return M.empty
        Just f  -> do f' <- (++ "/" ++ f) <$> getCurrentDirectory
                      fe <- doesFileExist f'
                      if fe
                        then do s <- System.IO.readFile f'
                                return $ (read s :: Users)
                        else return M.empty

    -- Create our "persistent" user/deploy list.
    putStrLn "Starting up with super user: "
    print users
    usersVar <- atomically $ newTVar users

    let nothingIfNull m = case m of
                              Nothing -> Nothing
                              Just "" -> Nothing
                              Just b  -> Just b

    -- Start up our good old scotty and give him some routes.
    let r = runLift . flip runReader usersVar
                    . flip runReader cfg

    scottyT port r r $ do
        -- Misc
        get "/" $ html $ "hello"
        get "/auth-check" $ do
            lvl <- param "lvl"
            withAuthLvl lvl $ text "okay"

        -- Users
        get "/users" $ do
            users' <- liftE . atomically . readTVar =<< askE
            text $ LT.fromStrict $ quantifyUsers $ M.toList users'

        get "/users.txt" $ do
            withAuthLvl 0 $ do
                (users' :: Users) <- liftE . atomically . readTVar =<< askE
                text $ LT.pack $ show users'

        get "/user" $ (liftE $ Data.Text.IO.readFile "static/new-user.html")
            >>= html . LT.fromStrict

        post "/user" postUserRoute

        -- Querying
        get "/list" $ do
            bucket  <- param "bucket"
            mprefix <- nothingIfNull <$> optionalParam "prefix"
            mdelim  <- nothingIfNull <$> optionalParam "delimiter"
            withCreds $ \c -> do infos <- liftE $ listDirectory c bucket mprefix mdelim
                                 text $ LT.intercalate "\n" $ Prelude.map (LT.fromStrict . objectKey) infos

        -- Uploading new files
        post "/upload" postUploadRoute

        -- Copying existing files
        get "/copy" $ do
            bucket <- param "bucket"
            from   <- param "from"
            to     <- param "to"
            withCreds $ \c -> do void $ liftE $ copyFile c bucket from to
                                 text "okay"

        get "/copy-folder" $ do
            bucket <- param "bucket"
            from   <- param "from"
            to     <- param "to"
            withCreds $ \c -> do void $ liftE $ copyDirectory c bucket from to
                                 text "okay"

        -- Catchall
        notFound $ text "not found 404"

postUserRoute = do
    name     <- param "name"
    pass     <- param "pass"
    addlvl   <- param "lvl"
    users'   <- liftE . atomically . readTVar =<< askE
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

               msg  <- askE >>= \users -> liftE $
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
    users <- askE

    mCreds <- liftE $ atomically $ (getAwsCreds name pass buck) <$> readTVar users

    case mCreds of
        Nothing -> html "invalid pass"
        Just c  -> do fs <- files
                      ts <- forM fs (liftE . uploadFile c buck ctype cenc acl)
                      html $ LT.fromStrict $ T.concat ts

askE :: (MT.MonadTrans m, Member (Reader a) r, Typeable a) => m (Eff r) a
askE = MT.lift ask

liftE :: (MT.MonadTrans m, SetMember Lift (Lift IO) r) => IO a -> m (Eff r) a
liftE = MT.lift . lift
