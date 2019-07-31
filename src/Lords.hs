{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords
    ( PL8(..)
    , Pallet
    , PixelRGBA8(..)
    , Tile(..)
    , parsePL8
    , parsePallet
    , convertFiles
    , convertUsingDuoList
    )
  where

import Codec.Picture.Png (encodePng)
import Codec.Picture.Types
    ( Image
    , MutableImage
    , newMutableImage
    , unsafeFreezeImage
    , writePixel
    )
import qualified Codec.Picture.Types as Picture
import Control.Applicative (pure)
import Control.Monad ((>>=), fail, mapM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST (RealWorld)
import Control.Monad.Par.IO (runParIO)
import Control.Monad.Par.Class (fork)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (writeFile)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Foldable (maximum)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Int (Int)
import qualified Data.Map as M ((!))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Vector.Unboxed ((!), Vector, head, splitAt)
import Data.Word (Word8)
import Prelude ((-), (+), fromIntegral)
import System.FilePath ((</>), replaceExtensions)
import System.IO (IO, FilePath, putStrLn)

import Lords.Duo (Duo (Duo, palletFile, pl8File), DuoList, parseDuo)
import Lords.PalletMap (PalletMap, createPalletMap)
import Lords.Pallet.Parser (parsePallet)
import Lords.Pallet.Types (Pallet, PixelRGBA8(PixelRGBA8, r, g, b, a))
import Lords.PL8.Parser (parsePL8)
import Lords.PL8.Types
    ( PL8(PL8, numberOfTiles, tiles)
    , Tile
        ( Tile
        , width
        , height
        , offset
        , x
        , y
        , extraType
        , extraRows
        , indicesToPallet
        )
    )


assetPath :: FilePath
assetPath = "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/"

{-# INLINE convertFiles #-}
convertFiles :: PalletMap -> DuoList -> IO ()
convertFiles palletMap = runParIO . mapM_ (fork . liftIO . convertFile)
  where
    convertFile :: Duo -> IO ()
    convertFile Duo{..} = do
        let pallet = palletMap M.! palletFile
        let fileName = assetPath </> pl8File
        let outFileName =
                "/home/yrid/pokus2/" </> replaceExtensions pl8File "png"
        convertToRgb pallet fileName outFileName

convertUsingDuoList :: IO ()
convertUsingDuoList = do
    duoList <- parseDuo "duo-list.json" >>= eitherToError
    palletMap <- createPalletMap duoList assetPath
    convertFiles palletMap duoList

{-# INLINE convertToRgb #-}
convertToRgb :: Pallet -> FilePath -> FilePath -> IO ()
convertToRgb pallet input output = do
    putStrLn $ "Input file: " <> input
    putStrLn $ " Output file: " <> output
    file <- parsePL8 input >>= eitherToError
    image <- fileToRGBImage pallet file
    writeFile output . toStrict $ encodePng image

{-# INLINE eitherToError #-}
eitherToError :: Either String b -> IO b
eitherToError (Right a) = pure a
eitherToError (Left e) = fail e

fileToRGBImage :: Pallet -> PL8 -> IO (Image Picture.PixelRGBA8)
fileToRGBImage pallet PL8{..} = do
    let (width, height) = getSize tiles
    image <- newMutableImage width height
    mapM_ (fillPixels image) tiles
    unsafeFreezeImage image
  where
    makeCoordinates :: Tile Identity -> [(Int, Int)]
    makeCoordinates Tile{..} =
        [(x', y') | y' <- [y..y+height-1], x' <- [x..x+width-1]]

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
