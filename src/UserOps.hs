{-# LANGUAGE OverloadedStrings #-}
module UserOps where

import Types
import Control.Concurrent.STM
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
getAwsCreds name pass buck usrs =
    if getUserIsValid name pass usrs
    then do detail <- M.lookup name usrs
            M.lookup buck $ userCreds detail
    else Nothing

getUserIsValid :: UserName -> ByteString -> Users -> Bool
getUserIsValid name pass usrs =
    let mBool = do detail <- M.lookup name usrs
                   return $ validatePassword (userPass detail) pass
    in case mBool of
        Nothing -> False
        Just b  -> b

getUserLevel :: UserName -> M.Map UserName UserDetail -> Maybe Int
getUserLevel name usrs = userLevel <$> M.lookup name usrs

addUser :: UsersVar -> UserName -> Int -> ByteString -> Maybe Bucket -> Maybe AwsCreds -> IO Text
addUser usrs name lvl pass mbuck mcreds = do
    Just hpass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy pass
    mUser <- atomically $ M.lookup name <$> readTVar usrs
    let detail = case mUser of
                     Nothing -> UserDetail lvl name hpass M.empty
                     Just d  -> d
        mdetail = do buck <- mbuck
                     creds <- mcreds
                     return $ addCreds detail $ M.fromList [(buck, creds)]
        detail' = maybe detail id mdetail
    if validatePassword (userPass detail) pass
    then do atomically $ modifyTVar' usrs $ M.insert name detail'
            return "ok"
    else return "bummer"


