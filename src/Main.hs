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
import Network.HTTP.Client
import Network.HTTP.Client.TLS
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
    prt  <- lookupDefault 3000 cfg "port"
    -- Get our startup users file from the config.
    mSaveFile <- DC.lookup cfg "save-file"
    -- Possibly get a TLS certificate and key.
    mSrvCrt <- DC.lookup cfg "server-crt"
    mSrvKey <- DC.lookup cfg "server-key"
    -- Create our user map.
    (SaveState usrs cfds) <- case mSaveFile of
        Nothing -> return $ SaveState M.empty M.empty
        Just f  -> do fe <- doesFileExist $ cwd </> f
                      if fe
                        then do s <- System.IO.readFile $ cwd </> f
                                return $ (read s :: SaveState)
                        else return $ SaveState M.empty M.empty

    -- Create our "persistent" user/deploy list.
    usersVar <- atomically $ newTVar usrs
    logVar   <- atomically $ newTVar $ Log []
    uidVar   <- atomically $ newTVar $ UniqueId 0
    tasksVar <- atomically $ newTVar $ M.empty
    cfdsVar  <- atomically $ newTVar cfds

    -- Group both certs because if one is missing their point is lost.
    let mSK = do crt <- mSrvCrt
                 key <- mSrvKey
                 return (crt,key)

    -- Create a new http manager for aws requests.
    mngr <- newManager $
                maybe defaultManagerSettings (const tlsManagerSettings) mSK


    -- Start up our good old scotty and give him some routes.
    let r = flip runReaderT (Pusher logVar usersVar cfdsVar cfg uidVar tasksVar mngr)
        f = do putStrLn "Running HTTP in the clear, SSL NOT enabled."
               scottyT prt r r routes
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
                                 scottyTTLS prt k c r r routes
                         else p crtExists keyExists >> f
