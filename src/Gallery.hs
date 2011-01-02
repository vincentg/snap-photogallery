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
import  Graphics.Transform.Magick.Types (HImage, getImage, columns, rows)
import  Foreign.Storable
import	Monad

data Dimension = Dimension { width :: Word, height :: Word }

-- | This method create all the thumbnails of a given album
createAlbumThumbs :: FilePath -- ^ Path of the album directory
                  -> FilePath -- ^ Suffix to be applied to the album path to store the thumbs
                  -> IO () 
createAlbumThumbs srcDir thumbSuffix = do
-- TODO log the exception
    files <- catch (listPicturesRM srcDir) (\e -> return [])
    if (not $ null files) 
      then do 
        createDirectoryIfMissing True $ srcDir </> thumbSuffix
        initializeMagick
       -- forM_ 
      else
-- TODO Add error management here, it means that an IO error occured, or that no image was in the album
        return()

-- | This method create the thumbnails of a given image
thumbImage :: FilePath -- ^ Path of the image to thumbnail
           -> FilePath -- ^ Path of the destination dir
           -> IO ()
thumbImage imagePath destDir = do
    sourceImg <- readImage imagePath
    thumbs <- calcReductionFactors sourceImg
    let (path, ext) = splitExtension imagePath
    let outputs = map (\x -> (uncurry max x, thumbnailImage (fst x) (snd x) sourceImg)) thumbs
    mapM_ (\x -> writeImage (composePath (fst x) path ext) (snd x)) outputs
  where
    composePath size path ext = destDir </> generateFileName (takeFileName path) size <.> ext
    generateFileName name size = name ++ "_th_" ++ show size ++ "px"


calcReductionFactors :: HImage -- ^ The image to thumbnail
                     -> IO [(Word, Word)] -- 
calcReductionFactors himg = do
    img <- peek $ getImage himg
    let colz = fromIntegral $ columns img
    let rowz = fromIntegral $ rows img
    let ratio = rowz % colz
    let scales = [1024, 720, 100]
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
    concatPath x y = normalise $ x </> y


listPicturesRM :: FilePath -> IO [FilePath]
listPicturesRM path = do
    cur_path <- getCurrentDirectory
    files <- recurseDir SystemFS $ normalise $ cur_path </> path
    filterM doesFileExist files


