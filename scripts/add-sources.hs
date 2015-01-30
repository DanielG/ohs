#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.List
import System.FilePath
import System.Directory
import System.Environment
import System.Process

max_depth = 3

getDirectoryContents' d = do
  exists <- doesDirectoryExist d
  if not exists
  then return []
  else
    map (d</>) . filter (not . flip elem [".", ".."]) <$> getDirectoryContents d

main = do
  b <- doesFileExist "ohs.cabal"
  when (not b)
    $ fail "./ohs.cabal doesn't exist, please run me in the project root."

  root <- canonicalizePath "."
  fs <- getDirectoryContents' =<< canonicalizePath "submodules"

  mapM_ (addSubmodule 0) =<< filterM doesDirectoryExist fs

addSubmodule depth _ | depth > max_depth = return ()
addSubmodule depth s = do
--  putStrLn $ "checking " ++ s
  fs <- getDirectoryContents' s
  let b = or $ do
            f <- fs
            return $ takeExtension f == ".cabal"

  case b of
    True -> do
           putStrLn $ "adding " ++ s
           system $ "cabal sandbox add-source" ++ s
    False -> do
--      putStrLn $ "no cabal file in: " ++ s
      addSubmodule (depth+1) `mapM_` fs
