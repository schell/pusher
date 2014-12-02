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
import System.Directory
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
                        then do s <- readFile f'
                                return $ (read s :: Users)
                        else return M.empty

    -- Create our "persistent" user/deploy list.
    putStrLn "Starting up with super user: "
    print users
    usersVar <- atomically $ newTVar users

    -- Start up our good old scotty and give him some routes.

    let r = runLift . flip runReader usersVar
                    . flip runReader cfg

    scottyT port r r $ do
        get "/" $ html $ "hello"
        get "/authCheck" $ do
            lvl <- param "lvl"
            withAuthLvl lvl $ html "okay"

        get "/users" $ do
            users' <- liftE . atomically . readTVar =<< askE
            html $ LT.fromStrict $ quantifyUsers $ M.toList users'

        get "/users.txt" $ do
            withAuthLvl 0 $ do
                (users' :: Users) <- liftE . atomically . readTVar =<< askE
                html $ LT.pack $ show users'

        postUserRoute
        postUploadRoute

        notFound $ html "not found 404"

postUserRoute = post "/user" $ do
    name     <- param "name"
    pass     <- param "pass"
    users'   <- liftE . atomically . readTVar =<< askE
    let addlvl  = maybe 10 id $ getUserLevel name users'
    withAuthLvl addlvl $ do
        buck <- param "bucket"
        key  <- param "key"
        secr <- param "secret"
        msg  <- askE >>= \users -> liftE $
            addUser users name addlvl pass buck $ AwsCreds key secr
        html $ LT.fromStrict msg

postUploadRoute = post "/upload" $ do
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
                      ts <- forM fs (liftE . uploadFile buck c ctype cenc acl)
                      html $ LT.fromStrict $ T.concat ts

askE :: (MT.MonadTrans m, Member (Reader a) r, Typeable a) => m (Eff r) a
askE = MT.lift ask

liftE :: (MT.MonadTrans m, SetMember Lift (Lift IO) r) => IO a -> m (Eff r) a
liftE = MT.lift . lift
