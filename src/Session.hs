{-# LANGUAGE OverloadedStrings #-}
module Session where

import Types
import Web.ClientSession
import Web.Scotty.Trans
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Time.Clock
import Data.Configurator
import Data.Maybe
import Data.Map as M
import qualified Control.Monad.Morph as MT
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as TL

type CookieMap = Map TL.Text TL.Text

readCfgCookieLife :: ActionP Int
readCfgCookieLife = do
    cfg     <- MT.lift $ asks pConfig
    -- Default sign-in cookie life is 30 days worth of seconds
    liftIO $ lookupDefault 2592000 cfg "sign-in-cookie-life"

renewUserCookie :: ActionP ()
renewUserCookie = do
    seconds <- readCfgCookieLife
    mUser <- readUserCookie
    flip (maybe (liftIO $ putStrLn "Can't renew cookie.")) mUser $ \(UserCookie uid _) -> do
        t <- liftIO $ getCurrentTime
        let t' = addUTCTime (fromIntegral seconds) t
        writeUserCookie $ UserCookie uid t'

expireUserCookie :: ActionP ()
expireUserCookie = do
    mUser <- readUserCookie
    flip (maybe (return ())) mUser $ \(UserCookie uid _) -> do
        t <- liftIO $ getCurrentTime
        let t' = addUTCTime (-60 * 10)  t
        writeUserCookie $ UserCookie uid t'

writeUserCookie :: UserCookie -> ActionP ()
writeUserCookie c@(UserCookie _ e) = do
    k  <- liftIO getDefaultKey
    u' <- liftIO $ encryptIO k $ B.pack $ show c
    t  <- liftIO $ getCurrentTime
    let life = diffUTCTime e t
    setHeader "Set-Cookie" $ TL.concat [ cookieName
                                       , "="
                                       , TL.pack $ B.unpack u'
                                       , "; "
                                       , "Path=/; "
                                       , "Max-Age="
                                       , TL.pack $ show life
                                       , "; HttpOnly"
                                       ]

readUserCookie :: ActionP (Maybe UserCookie)
readUserCookie = do
    -- Retrieve and parse our cookies.
    mCookies <- header "Cookie"
    -- Get our server's decryption key.
    k <- liftIO getDefaultKey
    -- Decode our UserCookie
    return $ do cookies <- fmap parseCookies mCookies
                cookie  <- M.lookup cookieName cookies
                cookieD <- decrypt k (B.pack $ TL.unpack cookie)
                maybeRead $ B.unpack cookieD
    where maybeRead = fmap fst . listToMaybe . reads

writeLoginCookie :: UserDetail -> ActionP ()
writeLoginCookie u = futureCookieForUser u >>= writeUserCookie

expireLoginCookie :: ActionP ()
expireLoginCookie = expireUserCookie

futureCookieForUser :: UserDetail -> ActionP UserCookie
futureCookieForUser u = do
    l <- readCfgCookieLife
    t <- liftIO $ getCurrentTime
    let t' = addUTCTime (fromIntegral l) t
    return $ UserCookie u t'

readUser :: ActionP (Maybe UserDetail)
readUser = do
    mCookie <- readUserCookie
    case mCookie of
        Nothing -> return Nothing
        Just (UserCookie u _) -> return $ Just u

cookieHasExpired :: UserCookie -> ActionP Bool
cookieHasExpired c = fmap (<= 0) $ cookieExpiresIn c

cookieExpiresIn :: UserCookie -> ActionP NominalDiffTime
cookieExpiresIn (UserCookie _ e) = do
    t <- liftIO getCurrentTime
    let d = diffUTCTime e t
    return d

cookieName :: TL.Text
cookieName = "cookie"

parseCookies :: TL.Text -> CookieMap
parseCookies = Prelude.foldl mapify M.empty . Prelude.map tuple . splitCookies
    where splitCookies   = TL.split (==';')
          tuple t        = (TL.takeWhile (/= '=') $ TL.dropWhile (== ' ') t, TL.drop 1 $ TL.dropWhile (/= '=') t)
          mapify m (k,v) = M.insert k v m


