{-# LANGUAGE OverloadedStrings #-}

{-|

This module takes care of the album management
-}


module Gallery.Album 
(
    Album(..),
    createAllAlbums
) where

import	System.FilePath
import  Gallery.Photo
import  Gallery.Base
import  Control.Monad

data Album = Album { 
               albumName :: String,
               albumPath :: FilePath,
               albumPhotos :: [Photo],
               albumDescription :: String
             } deriving Show

createAllAlbums :: FilePath -> IO [Album]
createAllAlbums root = do
    albums <- listAlbums root
    forM albums createAlbumM

createAlbumM :: FilePath -> IO Album
createAlbumM path = do
    -- Create thumbnails and returns the list of photos in the albums
    photos <- createAlbumThumbs path ".thumbs"
    return $ createAlbum path photos

createAlbum :: FilePath -> [FilePath] -> Album
createAlbum path photos = 
    Album 
      (takeBaseName path)
      path
      (map createPhoto photos)
      ""

