module Utils where

import System.Directory
import System.FilePath
import Control.Monad
import Debug.Trace

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = do
    fs <- recurse "" fp
    return $ map (drop $ length fp) fs
    where recurse :: FilePath -> FilePath -> IO [FilePath]
          recurse _ "." = return []
          recurse _ ".." = return []
          recurse p f = do
              let pf = p </> f
              dfe <- doesFileExist pf
              if trace (pf ++ " is file:" ++ show dfe) dfe
              then return [pf]
              else do dde <- doesDirectoryExist f
                      if trace (pf ++ " is dir:" ++ show dde) dde
                      then do fs'  <- getDirectoryContents f
                              fs'' <- forM fs' $ recurse pf
                              return $ concat fs''
                      else return []
