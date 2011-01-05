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
import  Data.Char (toLower)
import  Graphics.Transform.Magick.Images
import  Graphics.Transform.Magick.Types (HImage, getImage, columns, rows)
import  Foreign.Storable
import	Monad
import	Control.Monad

data Dimension = Dimension { width :: Word, height :: Word }

-- | Returns the maximum size between the width and the edeg
getMax :: Dimension -> Word 
getMax (Dimension x y) = max x y


createAllThumbs :: FilePath -- ^ Albums path (support a path from the current directory or from the root)
                -> FilePath -- ^ Suffix to be applied to the album path to store the thumbs
                -> IO ()
createAllThumbs root suffix = do
    initializeMagick
    albums <- listAlbumsM root
    forM_ albums (`createAlbumThumbs` suffix)

-- | This method create all the thumbnails of a given album
createAlbumThumbs :: FilePath -- ^ Path of the album directory
                  -> FilePath -- ^ Suffix to be applied to the album path to store the thumbs
                  -> IO () 
createAlbumThumbs srcDir thumbSuffix = do
-- TODO log the exception
    files <- catch (listPicturesRM srcDir) (\e -> return [])
    if (not $ null files) 
      then do 
        let destDir = srcDir </> thumbSuffix
        createDirectoryIfMissing True $ destDir
        forM_ files (thumbImage destDir)
      else
-- TODO Add error management here, it means that an IO error occured, or that no image was in the album
        return()

-- | This method create the thumbnails of a given image
thumbImage :: FilePath -- ^ Path of the destination dir
           -> FilePath -- ^ Path of the image to thumbnail 
           -> IO ()
thumbImage destDir imagePath = do
    sourceImg <- readImage imagePath
    sizes <- calcReductionFactors sourceImg
    let (path, ext) = splitExtension imagePath
    forM_ sizes $ \s -> do
      let thumb = thumbnailImage (width s) (height s) sourceImg
      writeImage (composePath (getMax s) path ext) thumb
      destroyImage thumb
    -- FIXME debug
    putStrLn $ imagePath ++ " Thumbnails generated ---"
    destroyImage sourceImg
  where
    composePath size path ext = destDir </> generateFileName (takeFileName path) size <.> ext
    generateFileName name size = name ++ "_th_" ++ show size ++ "px"

calcReductionFactors :: HImage -- ^ The image to thumbnail
                     -> IO [Dimension] -- 
calcReductionFactors himg = do
    img <- peek $ getImage himg
    return $ computeFactors (columns img) (rows img)

computeFactors :: (Integral a) => a -> a -> [Dimension]
computeFactors col row =
    let colz = fromIntegral $ col
        rowz = fromIntegral $ row
        ratio = rowz % colz
        scales = [1024, 720, 100]
    in
    if colz > rowz
        then
        map (\x -> Dimension {
                     width=(truncate x),
                     height=(truncate $ x * ratio)
                  }
            ) scales
        else
        map (\x -> Dimension {
                     width=(truncate $ x * 1/ratio),
                     height=(truncate x)
                  }
            ) scales

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
      mapM (return . concatPath fullPath) >>=
      filterM doesDirectoryExist
  where
    concatPath x y = normalise $ x </> y


listPicturesRM :: FilePath -> IO [FilePath]
listPicturesRM path = do
    let formats = [".jpg",".png",".gif"]
    cur_path <- getCurrentDirectory
    (recurseDir SystemFS $ normalise $ cur_path </> path) >>=
      -- filter only files with a supported extension
      filterM (return . (`elem` formats) . takeExtension . map toLower) >>=
      filterM doesFileExist 


--TEST MAIN FUNCTION
main :: IO ()
main = do
    putStrLn "Enter root directory (where the albums are located)"
    root <- getLine
    putStrLn "Enter suffix to be used for the thumbnails"
    suffix <- getLine
    putStrLn "Thx"
    createAllThumbs root suffix
    putStrLn "All Thumbs are created"
   
    

