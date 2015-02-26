{- For generating a self signed certificate to run this server on TLS,
   @see https://github.com/yesodweb/wai/tree/master/warp-tls
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Routes
import System.Environment
import System.IO
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath
import Web.Scotty.Trans hiding (get, post)
import Web.Scotty.TLS
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Trans.Reader
import Data.Configurator.Types
import Data.Configurator as DC
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    -- Get the config file from the command line.
    args <- getArgs
    cfg <- case args of
        []  -> return $ DC.empty
        f:_ -> fst <$> autoReload autoConfig [Required f]

    cwd <- getCurrentDirectory

    -- Get our port.
    port  <- lookupDefault 3000 cfg "port"
    -- Get our startup users file from the config.
    mUsersFile <- DC.lookup cfg "users-file"
    -- Possibly get a TLS certificate and key.
    mSrvCrt <- DC.lookup cfg "server-crt"
    mSrvKey <- DC.lookup cfg "server-key"
    -- Create our user map.
    users <- case mUsersFile of
        Nothing -> return M.empty
        Just f  -> do fe <- doesFileExist $ cwd </> f
                      if fe
                        then do s <- System.IO.readFile $ cwd </> f
                                return $ (read s :: Users)
                        else return M.empty

    -- Create our "persistent" user/deploy list.
    putStrLn "Starting up with super user: "
    print users
    usersVar <- atomically $ newTVar users
    logVar   <- atomically $ newTVar $ Log []
    uidVar   <- atomically $ newTVar $ UniqueID 0

    -- Group both certs because if one is missing their point is lost.
    let mSK = do crt <- mSrvCrt
                 key <- mSrvKey
                 return (crt,key)

    -- Start up our good old scotty and give him some routes.
    let r = flip runReaderT (Pusher logVar usersVar cfg uidVar)
        f = do putStrLn "Running HTTP in the clear, SSL NOT enabled."
               scottyT port r r routes
        p c k = putStrLn $ unwords [ "SSL Certificate"
                                   , if c then "does" else "does not"
                                   , "exist. SSL Key"
                                   , if k then "does" else "does not"
                                   , "exist. "
                                   ]
    case mSK of
        Nothing    -> f
        Just (c,k) -> do crtExists <- doesFileExist c
                         keyExists <- doesFileExist k
                         if crtExists && keyExists
                         then do putStrLn "SSL Enabled."
                                 scottyTTLS port k c r r routes
                         else p crtExists keyExists >> f
