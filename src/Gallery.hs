{-# LANGUAGE OverloadedStrings #-}

{-|

This module takes care of the image management, album listing, and thumbnailing

-}


module Gallery
(
) where

import	System.IO.HVFS.Utils
import	System.Directory
import	System.FilePath
import	Data.String.Utils
import  Data.Ratio
import  Data.Word
import  Graphics.Transform.Magick.Images
import  Graphics.Transform.Magick.Types 
import  Foreign.Storable
import	Monad

-- | This method create the thumbnails of a given image
createAlbumThumbs :: FilePath -- ^ Path of the image to thumbnail
                  -> FilePath -- ^ Path of the destination dir
                  -> IO () -- ^ Return value  (can be an exception) 
createAlbumThumbs srcDir dstDir = do
    files <- catch (listFilesRM srcDir) (\e -> return [])
    initializeMagick

-- Works !! but thumbnail directory creation is to be done before writing the thumbnailed image
thumbImage :: FilePath 
           -> FilePath
           -> IO [()]
thumbImage imagePath destDir = do
    sourceImg <- readImage imagePath
    thumbs <- calcReductionFactors sourceImg
    let (path,ext) = splitExtension imagePath
    let outputs = map (\x -> (uncurry max x, thumbnailImage (fst x) (snd x) sourceImg)) thumbs
    mapM (\x -> writeImage (composePath (fst x) path ext) (snd x) ) outputs
  where
    composePath size path ext = normalise $ combine destDir (addExtension (generateFileName (takeFileName path) size) ext)
    generateFileName name size = name ++ "_th_" ++ show size ++ "px"
 -- let destImg = thumbnailImage


calcReductionFactors ::
		     HImage
                     -> IO [(Word, Word)]
calcReductionFactors himg = do
    img <- peek $ getImage himg
    let colz = fromIntegral $ columns img
    let rowz = fromIntegral $ rows img
    let ratio = (%) rowz colz
    let scales = [1024, 640, 100]
    if colz > rowz
      then
        return $ map (\x -> (truncate x , truncate $ x * ratio)) scales
      else
        return $ map (\x -> (truncate $ x * 1/ratio, truncate x)) scales

       
{-
 IO errors are to be catched, like catch f (\e -> return [])
-}
listAlbumsM :: FilePath -> IO [FilePath]
listAlbumsM path = do
    cur_dir <- getCurrentDirectory 
    let fullPath = concatPath cur_dir path
    (getDirectoryContents $ fullPath) >>= 
      -- does not returns files that start with a dot, and only directories
      filterM (return . (/= '.') . head) >>= 
      filterM (doesDirectoryExist . concatPath fullPath)
  where
    concatPath x y = normalise $ combine x y

listFilesRM :: FilePath -> IO [FilePath]
listFilesRM path = do
    cur_path <- getCurrentDirectory
    files <- recurseDir SystemFS $ normalise $ combine cur_path path
    filterM doesFileExist files


