{-# LANGUAGE TupleSections #-}
module System.Pusher where

import System.Directory
import System.FilePath
import Aws
import Control.Monad
import Data.Text (Text)

loadAwsCreds :: Text -> IO (Maybe (FilePath, Credentials))
loadAwsCreds key = do
    dirs   <- getDirectories
    mcreds <- mapM (flip loadCredentialsFromFile key . (</> ".aws-keys")) dirs
    return $ msum $ zipWith (\d m -> (d,) <$> m) dirs mcreds

getDirectories :: IO [FilePath]
getDirectories = getCurrentDirectory >>= return . subs
    where subs "/" = ["/"]
          subs fp = fp : (subs $ takeDirectory fp)
