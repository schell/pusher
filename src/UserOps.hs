{-# LANGUAGE OverloadedStrings #-}
module UserOps where

import Types
import Control.Concurrent.STM
import Control.Applicative
import Crypto.BCrypt
import Aws.S3
import Data.Text as T
import Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

updatePassword :: UserDetail -> ByteString -> IO UserDetail
updatePassword user pass = do
    Just hpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy pass
    return $ user{userPass = hpass}

userBuckets :: UserDetail -> [Bucket]
userBuckets = M.keys . userCreds

addCreds :: UserDetail -> M.Map Bucket AwsCreds -> UserDetail
addCreds (UserDetail lvl name pass creds) creds' =
    UserDetail lvl name pass $ M.union creds' creds

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

addUser :: UsersVar -> UserName -> Int -> ByteString -> Maybe Bucket -> Maybe AwsCreds -> IO Text
addUser users name lvl pass mbuck mcreds = do
    Just hpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy pass
    mUser <- atomically $ M.lookup name <$> readTVar users
    let detail = case mUser of
                     Nothing -> UserDetail lvl name hpass M.empty
                     Just d  -> d
        mdetail = do buck <- mbuck
                     creds <- mcreds
                     return $ addCreds detail $ M.fromList [(buck, creds)]
        detail' = maybe detail id mdetail
    if validatePassword (userPass detail) pass
    then do atomically $ modifyTVar' users $ M.insert name detail'
            return "ok"
    else return "bummer"


