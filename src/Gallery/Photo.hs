{-# LANGUAGE OverloadedStrings #-}

{-|

This module takes care of the pictures management
-}


module Gallery.Photo
(
    Photo (..),
    createPhoto
) where

import System.FilePath

data Photo = Photo { 
                 photoName :: String,
                 photoPath :: FilePath,
                 photoDescription :: String
               } deriving Show

createPhoto :: FilePath -> Photo
createPhoto path = Photo (takeBaseName path) path ""

