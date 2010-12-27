{-# LANGUAGE OverloadedStrings #-}

{-|

This module takes care of the image management, album listing, and thumbnailing

-}


module Gallery
  (
  ) where

import System.IO.HVFS.Utils
import System.Directory
import System.FilePath
import Monad


listFilesR :: FilePath -> IO [FilePath]
listFilesR path = do
  cur_path <- getCurrentDirectory
  files <- recurseDir SystemFS $ normalise $ combine cur_path path
  filterM doesFileExist files
