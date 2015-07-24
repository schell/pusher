{-# LANGUAGE TupleSections #-}
module System.Pusher where

import System.Directory
import System.FilePath
import Aws
import Control.Monad
import Data.Text (Text)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- | Loads the aws creds by keyname from the currenty directory or parent
-- directories.
loadAwsCreds :: Text -> IO (Maybe (FilePath, Credentials))
loadAwsCreds key = do
    dirs   <- getDirectories
    mcreds <- mapM (flip loadCredentialsFromFile key . (</> ".aws-keys")) dirs
    mjcred <- loadJSONCreds
    return $ msum $ mjcred : zipWith (\d m -> (d,) <$> m) dirs mcreds

loadJSONCreds :: IO (Maybe (FilePath, Credentials))
loadJSONCreds = do
    dirs <- getDirectories
    mcreds <- forM dirs $ \dir -> do
        let file = dir </> "deploy-auth.json"
        exists <- doesFileExist file
        if exists
        then do mcreds <- readJSONCreds file
                return $ (file,) <$> mcreds
        else return Nothing
    return $ msum mcreds

readJSONCreds :: FilePath -> IO (Maybe Credentials)
readJSONCreds file = do
    str <- B.readFile file
    case decodeStrict str of
        Nothing -> return Nothing
        Just m  -> do let f = do key <- M.lookup "accessKeyId" m
                                 sec <- M.lookup "secretAccessKey" m
                                 return $ makeCredentials (B.pack key) (B.pack sec)
                      sequence f
-- | Returns the current working directory and each parent directory.
getDirectories :: IO [FilePath]
getDirectories = getCurrentDirectory >>= return . subs
    where subs "/" = ["/"]
          subs fp = fp : (subs $ takeDirectory fp)

-- | Checks a file as an absolute path and relative path - if either path
-- is a valid file then returns a Just filepath.
checkFilePath :: FilePath -> IO (Maybe FilePath)
checkFilePath fp = do
    isAbs <- doesFileExist fp
    if isAbs
    then return $ Just fp
    else do
        cwd   <- getCurrentDirectory
        let relfp = cwd </> fp
        isRel <- doesFileExist relfp
        if isRel
        then return $ Just relfp
        else return $ Nothing
