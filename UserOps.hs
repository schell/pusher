{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module UserOps where

import Control.Concurrent.STM
import Control.Applicative
import Crypto.BCrypt
import Aws.S3
import Data.Typeable
import Data.Text as T
import Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

data AwsCreds = AwsCreds { awsKey :: ByteString
                         , awsSecret :: ByteString
                         } deriving (Show, Read)

type UserName = Text

data UserDetail = UserDetail { userLevel :: Int
                             , userPass  :: ByteString
                             , userCreds :: M.Map Bucket AwsCreds
                             } deriving (Show, Read, Typeable)

type Users = M.Map UserName UserDetail
type UsersVar = TVar Users

addCreds :: UserDetail -> M.Map Bucket AwsCreds -> UserDetail
addCreds (UserDetail lvl pass creds) creds' =
    UserDetail lvl pass $ M.union creds' creds

getAwsCreds :: UserName -> ByteString -> Bucket -> Users -> Maybe AwsCreds
getAwsCreds name pass buck users =
    if getUserIsValid name pass users
    then do detail <- M.lookup name users
            M.lookup buck $ userCreds detail
    else Nothing

getUserIsValid :: UserName -> ByteString -> Users -> Bool
getUserIsValid name pass users =
    let mBool = do detail <- M.lookup name users
                   return $ validatePassword (userPass detail) pass
    in case mBool of
        Nothing -> False
        Just b  -> b

getUserLevel :: UserName -> M.Map UserName UserDetail -> Maybe Int
getUserLevel name users = userLevel <$> M.lookup name users

addUser :: UsersVar -> UserName -> Int -> ByteString -> Bucket -> AwsCreds -> IO Text
addUser users name lvl pass buck creds = do
    Just hpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy pass
    mUser <- atomically $ M.lookup name <$> readTVar users
    let detail = case mUser of
                     Nothing -> UserDetail lvl hpass M.empty
                     Just d  -> d
        detail' = addCreds detail $ M.fromList [(buck, creds)]
    if validatePassword (userPass detail) pass
    then do atomically $ modifyTVar' users $ M.insert name detail'
            return "ok"
    else return "bummer"

quantifyUsers :: [(UserName, UserDetail)] -> Text
quantifyUsers = quantify
    where quantify [] = ""
          quantify ((n, d):us) = T.unwords [ "user"
                                           , n
                                           , "with buckets"
                                           , T.intercalate ", " (M.keys $ userCreds d)
                                           , "\n"
                                           , quantify us
                                           ]
