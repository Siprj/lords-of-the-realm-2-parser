{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords
    ( PL8(..)
    , Pallet(..)
    , PixelRGBA8(..)
    , Tile(..)
    , parsePL8
    , parsePallet
    , convertFiles
    , work
    )
  where

import Codec.Picture.Png (PngSavable, encodePng)
import Codec.Picture.Types (Image, MutableImage)
import Codec.Picture.Types (newMutableImage, unsafeFreezeImage, writePixel)
import qualified Codec.Picture.Types as Picture
import Control.Applicative ((<*>), pure)
import Control.Monad ((>>=), fail, mapM_, unless)
import Control.Monad.ST (RealWorld)
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (writeFile, readFile)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (maximum)
import Data.Function (($), (.), flip)
import Data.Functor ((<$>), fmap)
import qualified Data.Map as M ((!))
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Int (Int)
import Data.List (nub, sort)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Vector.Unboxed ((!), Vector, head, splitAt)
import Data.Word (Word8)
import Prelude ((-), (+), zip, fromIntegral)
import System.FilePath.Glob (globDir1, compile)
import System.FilePath ((</>), (-<.>), replaceExtensions, replaceDirectory)
import System.IO (IO, FilePath, putStrLn)

import Lords.Pallet.Parser (parsePallet)
import Lords.Pallet.Types (Pallet, PixelRGBA8(..))
import Lords.PL8.Parser (parsePL8)
import Lords.PL8.Types (PL8(..), Tile(..))
import Lords.Duo (Duo(..), DuoList, parseDuo)
import Lords.PalletMap (PalletMap, createPalletMap)


assetPath = "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/"

{-# INLINE convertFiles #-}
convertFiles :: PalletMap -> DuoList -> IO ()
convertFiles palletMap duoList =
    mapM_ convertFile duoList
  where
    convertFile :: Duo -> IO ()
    convertFile Duo{..} = do
        let pallet = palletMap M.! palletFile
        let fileName = assetPath </> pl8File
        let outFileName =
                "/home/yrid/pokus2/" </> replaceExtensions pl8File "png"
        convertToRgb pallet fileName outFileName


    palletFile = "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/Base01.256"

work :: IO ()
work = do
    duoList <- parseDuo "asdff" >>= eitherToError
    palletMap <- createPalletMap duoList assetPath
    convertFiles palletMap duoList

convertFileDebug :: IO ()
convertFileDebug = do
    pallet <- parsePallet (assetPath </> "Base01.256") >>= eitherToError
    convertToRgb pallet (assetPath </> "Base1a.pl8") "/home/yrid/pokus2/Base01.png"


{-# INLINE convertToRgb #-}
convertToRgb :: Vector PixelRGBA8 -> FilePath -> FilePath -> IO ()
convertToRgb pallet input output =
    convertBy (fileToRGBImage pallet) input output

{-# INLINE convertToGray #-}
convertToGray :: Vector PixelRGBA8 -> FilePath -> FilePath -> IO ()
convertToGray pallet input output =
    convertBy (fileToGrayImage pallet) input output

{-# INLINE convertBy #-}
convertBy
    :: PngSavable a
    => (PL8 -> IO (Image a))
    -> FilePath
    -> FilePath
    -> IO ()
convertBy f input output = do
    putStrLn $ "Input file: " <> input
    putStrLn $ " Output file: " <> output
    file <- parsePL8 input >>= eitherToError
    image <- f file
--    writeMetaData file $ output -<.> "json"
    writeFile output . toStrict $ encodePng image

{-# INLINE eitherToError #-}
eitherToError :: Either String b -> IO b
eitherToError (Right a) = pure a
eitherToError (Left e) = fail e

fileToGrayImage :: Vector PixelRGBA8 -> PL8 -> IO (Image Picture.Pixel8)
fileToGrayImage _ PL8{..} = do
    let (width, height) = getSize tiles
    image <- newMutableImage width height
    mapM_ (fillPixels image) tiles
    unsafeFreezeImage image
  where
    makeCoordinates :: Tile Identity -> [(Int, Int)]
    makeCoordinates Tile{..} =
        [(x, y) | y <- [y..y+height-1], x <- [x..x+width-1]]

    fillPixels
        :: MutableImage RealWorld Picture.Pixel8
        -> Tile Identity
        -> IO ()
    fillPixels image tile@Tile{..} =
        fillPixels' image (runIdentity indicesToPallet) (makeCoordinates tile)

    fillPixels'
        :: MutableImage RealWorld Picture.Pixel8
        -> Vector Word8
        -> [(Int, Int)]
        -> IO ()
    fillPixels' image imageData ((x, y) : coordinates) = do
        let (pixel, imageData') = uncons imageData
        writePixel image x y $ pixel
        fillPixels' image imageData' coordinates
    fillPixels' _ _ [] = pure ()

    uncons vec = let (v, vs) = splitAt 1 vec in (head v, vs)


fileToRGBImage :: Vector PixelRGBA8 -> PL8 -> IO (Image Picture.PixelRGBA8)
fileToRGBImage pallet PL8{..} = do
    let (width, height) = getSize tiles
    image <- newMutableImage width height
    mapM_ (fillPixels image) tiles
    unsafeFreezeImage image
  where
    makeCoordinates :: Tile Identity -> [(Int, Int)]
    makeCoordinates Tile{..} =
        [(x, y) | y <- [y..y+height-1], x <- [x..x+width-1]]

    fillPixels
        :: MutableImage RealWorld Picture.PixelRGBA8
        -> Tile Identity
        -> IO ()
    fillPixels image tile@Tile{..} =
        fillPixels' image (runIdentity indicesToPallet) (makeCoordinates tile)

    fillPixels'
        :: MutableImage RealWorld Picture.PixelRGBA8
        -> Vector Word8
        -> [(Int, Int)]
        -> IO ()
    fillPixels' image imageData ((x, y) : coordinates) = do
        let (pixel, imageData') = uncons imageData
        unless (pixel == 0) $ writePixel image x y . toJuicypixels $ pallet ! fromIntegral pixel
        fillPixels' image imageData' coordinates
    fillPixels' _ _ [] = pure ()

    toJuicypixels :: PixelRGBA8 -> Picture.PixelRGBA8
    toJuicypixels PixelRGBA8{r, g, b, a} = Picture.PixelRGBA8 r g b a

    uncons vec = let (v, vs) = splitAt 1 vec in (head v, vs)

{-# INLINE getSize #-}
getSize :: [Tile a] -> (Int, Int)
getSize tiles =
    ( maximum $ fmap (\Tile{..} -> x + width) tiles
    , maximum $ fmap (\Tile{..} -> y + height) tiles
    )
