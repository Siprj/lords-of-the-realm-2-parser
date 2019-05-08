{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Codec.Picture.Png
import Codec.Picture.Types
import Control.Applicative
import Control.Monad hiding (replicateM)
import Data.ByteString hiding (putStrLn, zipWith, head, maximum, zip, replicate, length)
import Data.ByteString.Lazy (toStrict)
import Data.Either
import Data.Bits
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Proxy
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Serialize.Get
import Data.String
import Data.Traversable
import Data.Vector hiding (drop, take, maximum, mapM_, mapM, sequence, length)
import Data.Word (Word16, Word32, Word8)
import Debug.Trace
import Numeric
import qualified Data.List as L
import Prelude((-), (*), (+), (<), (==), compare, uncurry, undefined, fromIntegral, min)
import qualified Prelude as P
import System.FilePath
import System.FilePath.Glob
import System.IO (IO, putStrLn, print, FilePath)
import Text.Show (Show, show)


data Tile a = Tile
    { width :: Int
    , height :: Int
    , offset :: Word32
    , x :: Int
    , y :: Int
    , extraType :: Word32
    , extraRows :: Word32
    , unknowenByte1 :: Word32
    , unknowenByte2 :: Word32
    , indicesToPallet :: a (Vector Word8)
    }

tileHeaderToTile :: Tile Proxy -> Vector Word8 -> Tile Identity
tileHeaderToTile Tile{..} indices = Tile
    { width
    , height
    , offset
    , x
    , y
    , extraType
    , extraRows
    , unknowenByte1
    , unknowenByte2
    , indicesToPallet = Identity indices
    }

deriving instance (Show (a (Vector Word8))) => Show (Tile a)

data File = File
    { unknowenByte3 :: Word16
    , numberOfTiles :: Word16
    , unknowenByte4 :: Word32
    , tiles :: Vector (Tile Identity)
    }
  deriving (Show)

getPallet :: Get (Vector PixelRGB8)
getPallet = replicateM 256 getPixel
  where
    getPixel = PixelRGB8
        <$> fmap (2*) getWord8
        <*> fmap (2*) getWord8
        <*> fmap (2*) getWord8

getFileHeader :: Get (Word16, Word16, Word32)
getFileHeader = do
    unknowenByte3' <- getWord16le
    numberOfTiles' <- getWord16le
    unknowenByte4' <- getWord32le
    pure (unknowenByte3', numberOfTiles', unknowenByte4')

printFileHeaders :: IO ()
printFileHeaders = do
    files <- globDir1 (compile "*.pl8") "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/"
    headers <- sequence <$> mapM (\f -> fmap (fmap (magic f) . runGet getFileHeader) $ readFile f) files
    either putStrLn (mapM_ printHeader) headers
  where
    printHeader :: (Word16, Word16, Word32, String) -> IO ()
    printHeader (a, b, c, f) =
        putStrLn . prepend0x . showHex a . addSpace . prepend0x . showHex b
        . addSpace . prepend0x $ showHex c " " <> f
    addSpace s = " " <> s
    prepend0x s = "0x" <> s
    magic f (a, b, c) = (a, b, c, f)

convertFiles :: IO ()
convertFiles = do
    files <- L.sort <$> globDir1 (compile "*.pl8") "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/"
    let inOutFiles = P.zip files $ fmap
            ((flip replaceDirectory) "/home/yrid/pokus2/"
            . (flip replaceExtensions) "png") files
    mapM_ convert inOutFiles
  where
    convert (input, output) =
        convertToRgb input output "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/Base01.256"

eitherToError :: Either String b -> IO b
eitherToError (Right a) = pure a
eitherToError (Left e) = fail e

convertBy
    :: PngSavable a
    => (File -> IO (Image a))
    -> FilePath
    -> FilePath
    -> IO ()
convertBy f input output = do
    putStrLn $ "Input file: " <> input
    putStrLn $ " Output file: " <> output
    file <- runGet getFile <$> readFile input >>= eitherToError
    image <- f file
    writeFile output . toStrict $ encodePng image

convertToGray :: FilePath -> FilePath -> IO ()
convertToGray = convertBy fileToGrayImage

convertToRgb :: FilePath -> FilePath -> FilePath -> IO ()
convertToRgb input output palletFile = do
    p <- readPallet
    convertBy (fileToRGBImage (Just p)) input output
  where
    readPallet = readFile palletFile >>= (eitherToError . runGet getPallet)

getFile :: Get File
getFile = do
    unknowenByte3' <- getWord16le
    numberOfTiles' <- getWord16le
    unknowenByte4' <- getWord32le
    tiles' <- replicateM (fromIntegral numberOfTiles') getTileHeader
    finalTiles <- mapM (if testBit unknowenByte3' 0 then getRLETile else getTile) tiles'
    pure $ File unknowenByte3' numberOfTiles' unknowenByte4' finalTiles
  where
    getTile :: Tile Proxy -> Get (Tile Identity)
    getTile header@Tile{..} = do
        kwa <- remaining
        let foo = show (height * width) <> " kwa: " <> show kwa <> "\n"
        indices <- getByteString $ trace foo $ min (height * width) kwa
        pure $ tileHeaderToTile header . fromList . unpack $ indices

    getRLETile :: Tile Proxy -> Get (Tile Identity)
    getRLETile header@Tile{..} =
        tileHeaderToTile header <$> getRLEData (fromIntegral (height * width))

getRLEData :: Int -> Get (Vector Word8)
getRLEData size = go mempty
  where
    go :: Vector Word8 -> Get (Vector Word8)
    go pixels = if length pixels < size
        then readData pixels
        else pure pixels

    readData pixels = do
        numOfOpaquePixels <- fromIntegral <$> getWord8
        if numOfOpaquePixels == 0
            then do
                numberOfTransparetnPixels <- fromIntegral <$> getWord8
                go $ pixels <> replicate numberOfTransparetnPixels 0
            else do
                data' <- fromList . unpack <$> getByteString numOfOpaquePixels
                go $ pixels <> data'

getTileHeader :: Get (Tile Proxy)
getTileHeader = Tile
    <$> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> getWord32le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord8
    <*> fmap fromIntegral getWord8
    <*> fmap fromIntegral getWord8
    <*> fmap fromIntegral getWord8
    <*> pure Proxy

getSize :: Vector (Tile a) -> (Int, Int)
getSize tiles =
    ( maximum $ fmap (\Tile{..} -> x + width) tiles
    , maximum $ fmap (\Tile{..} -> y + height) tiles
    )

fileToGrayImage :: File -> IO (Image Pixel8)
fileToGrayImage File{..} = do
    let (width, height) = traceShowId $ getSize tiles
    image <- newMutableImage @Pixel8 width height
    mapM_ (fillPixel image) . fold $ fmap dataWihtCoordiantes tiles
    unsafeFreezeImage image
  where
    dataWihtCoordiantes :: Tile Identity -> Vector (Word8, (Int, Int))
    dataWihtCoordiantes Tile{..} =
        zip (runIdentity indicesToPallet) $ fromList coordinates
      where
        coordinates = [(x, y) | y <- [y..y+height], x <- [x..x+width]]

    fillPixel image (pixel, (x, y)) = writePixel image x y pixel

fileToRGBImage :: Maybe (Vector PixelRGB8) -> File -> IO (Image PixelRGB8)
fileToRGBImage maybePallet File{..} = do
    let (width, height) = traceShowId $ getSize tiles
    image <- newMutableImage @PixelRGB8 width height
    mapM_ (fillPixel image) . fold $ fmap dataWihtCoordiantes tiles
    unsafeFreezeImage image
  where
    dataWihtCoordiantes :: Tile Identity -> Vector (Word8, (Int, Int))
    dataWihtCoordiantes Tile{..} =
        zip (runIdentity indicesToPallet) $ fromList coordinates
      where
        coordinates = [(x, y) | y <- [y..y+height-1], x <- [x..x+width-1]]

    fillPixel image (pixel, (x, y)) = writePixel image x y
        $ fromMaybe defaultPallet maybePallet ! fromIntegral pixel

    defaultPallet = fromList
        [ PixelRGB8 0 0 0
        , PixelRGB8 0 0 128
        , PixelRGB8 0 128 0
        , PixelRGB8 0 128 128
        , PixelRGB8 128 0 0
        , PixelRGB8 128 0 128
        , PixelRGB8 128 128 0
        , PixelRGB8 192 192 192
        , PixelRGB8 192 220 192
        , PixelRGB8 200 208 212
        , PixelRGB8 0 32 64
        , PixelRGB8 0 32 96
        , PixelRGB8 0 32 128
        , PixelRGB8 0 32 160
        , PixelRGB8 0 32 192
        , PixelRGB8 0 32 224
        , PixelRGB8 0 64 0
        , PixelRGB8 0 64 32
        , PixelRGB8 0 64 64
        , PixelRGB8 0 64 96
        , PixelRGB8 0 64 128
        , PixelRGB8 0 64 160
        , PixelRGB8 0 64 192
        , PixelRGB8 0 64 224
        , PixelRGB8 0 96 0
        , PixelRGB8 0 96 32
        , PixelRGB8 0 96 64
        , PixelRGB8 0 96 96
        , PixelRGB8 0 96 128
        , PixelRGB8 0 96 160
        , PixelRGB8 0 96 192
        , PixelRGB8 0 96 224
        , PixelRGB8 0 128 0
        , PixelRGB8 0 128 32
        , PixelRGB8 0 128 64
        , PixelRGB8 0 128 96
        , PixelRGB8 0 128 128
        , PixelRGB8 0 128 160
        , PixelRGB8 0 128 192
        , PixelRGB8 0 128 224
        , PixelRGB8 0 160 0
        , PixelRGB8 0 160 32
        , PixelRGB8 0 160 64
        , PixelRGB8 0 160 96
        , PixelRGB8 0 160 128
        , PixelRGB8 0 160 160
        , PixelRGB8 0 160 192
        , PixelRGB8 0 160 224
        , PixelRGB8 0 192 0
        , PixelRGB8 0 192 32
        , PixelRGB8 0 192 64
        , PixelRGB8 0 192 96
        , PixelRGB8 0 192 128
        , PixelRGB8 0 192 160
        , PixelRGB8 0 192 192
        , PixelRGB8 0 192 224
        , PixelRGB8 0 224 0
        , PixelRGB8 0 224 32
        , PixelRGB8 0 224 64
        , PixelRGB8 0 224 96
        , PixelRGB8 0 224 128
        , PixelRGB8 0 224 160
        , PixelRGB8 0 224 192
        , PixelRGB8 0 224 224
        , PixelRGB8 64 0 0
        , PixelRGB8 64 0 32
        , PixelRGB8 64 0 64
        , PixelRGB8 64 0 96
        , PixelRGB8 64 0 128
        , PixelRGB8 64 0 160
        , PixelRGB8 64 0 192
        , PixelRGB8 64 0 224
        , PixelRGB8 64 32 0
        , PixelRGB8 64 32 32
        , PixelRGB8 64 32 64
        , PixelRGB8 64 32 96
        , PixelRGB8 64 32 128
        , PixelRGB8 64 32 160
        , PixelRGB8 64 32 192
        , PixelRGB8 64 32 224
        , PixelRGB8 64 64 0
        , PixelRGB8 64 64 32
        , PixelRGB8 64 64 64
        , PixelRGB8 64 64 96
        , PixelRGB8 64 64 128
        , PixelRGB8 64 64 160
        , PixelRGB8 64 64 192
        , PixelRGB8 64 64 224
        , PixelRGB8 64 96 0
        , PixelRGB8 64 96 32
        , PixelRGB8 64 96 64
        , PixelRGB8 64 96 96
        , PixelRGB8 64 96 128
        , PixelRGB8 64 96 160
        , PixelRGB8 64 96 192
        , PixelRGB8 64 96 224
        , PixelRGB8 64 128 0
        , PixelRGB8 64 128 32
        , PixelRGB8 64 128 64
        , PixelRGB8 64 128 96
        , PixelRGB8 64 128 128
        , PixelRGB8 64 128 160
        , PixelRGB8 64 128 192
        , PixelRGB8 64 128 224
        , PixelRGB8 64 160 0
        , PixelRGB8 64 160 32
        , PixelRGB8 64 160 64
        , PixelRGB8 64 160 96
        , PixelRGB8 64 160 128
        , PixelRGB8 64 160 160
        , PixelRGB8 64 160 192
        , PixelRGB8 64 160 224
        , PixelRGB8 64 192 0
        , PixelRGB8 64 192 32
        , PixelRGB8 64 192 64
        , PixelRGB8 64 192 96
        , PixelRGB8 64 192 128
        , PixelRGB8 64 192 160
        , PixelRGB8 64 192 192
        , PixelRGB8 64 192 224
        , PixelRGB8 64 224 0
        , PixelRGB8 64 224 32
        , PixelRGB8 64 224 64
        , PixelRGB8 64 224 96
        , PixelRGB8 64 224 128
        , PixelRGB8 64 224 160
        , PixelRGB8 64 224 192
        , PixelRGB8 64 224 224
        , PixelRGB8 128 0 0
        , PixelRGB8 128 0 32
        , PixelRGB8 128 0 64
        , PixelRGB8 128 0 96
        , PixelRGB8 128 0 128
        , PixelRGB8 128 0 160
        , PixelRGB8 128 0 192
        , PixelRGB8 128 0 224
        , PixelRGB8 128 32 0
        , PixelRGB8 128 32 32
        , PixelRGB8 128 32 64
        , PixelRGB8 128 32 96
        , PixelRGB8 128 32 128
        , PixelRGB8 128 32 160
        , PixelRGB8 128 32 192
        , PixelRGB8 128 32 224
        , PixelRGB8 128 64 0
        , PixelRGB8 128 64 32
        , PixelRGB8 128 64 64
        , PixelRGB8 128 64 96
        , PixelRGB8 128 64 128
        , PixelRGB8 128 64 160
        , PixelRGB8 128 64 192
        , PixelRGB8 128 64 224
        , PixelRGB8 128 96 0
        , PixelRGB8 128 96 32
        , PixelRGB8 128 96 64
        , PixelRGB8 128 96 96
        , PixelRGB8 128 96 128
        , PixelRGB8 128 96 160
        , PixelRGB8 128 96 192
        , PixelRGB8 128 96 224
        , PixelRGB8 128 128 0
        , PixelRGB8 128 128 32
        , PixelRGB8 128 128 64
        , PixelRGB8 128 128 96
        , PixelRGB8 128 128 128
        , PixelRGB8 128 128 160
        , PixelRGB8 128 128 192
        , PixelRGB8 128 128 224
        , PixelRGB8 128 160 0
        , PixelRGB8 128 160 32
        , PixelRGB8 128 160 64
        , PixelRGB8 128 160 96
        , PixelRGB8 128 160 128
        , PixelRGB8 128 160 160
        , PixelRGB8 128 160 192
        , PixelRGB8 128 160 224
        , PixelRGB8 128 192 0
        , PixelRGB8 128 192 32
        , PixelRGB8 128 192 64
        , PixelRGB8 128 192 96
        , PixelRGB8 128 192 128
        , PixelRGB8 128 192 160
        , PixelRGB8 128 192 192
        , PixelRGB8 128 192 224
        , PixelRGB8 128 224 0
        , PixelRGB8 128 224 32
        , PixelRGB8 128 224 64
        , PixelRGB8 128 224 96
        , PixelRGB8 128 224 128
        , PixelRGB8 128 224 160
        , PixelRGB8 128 224 192
        , PixelRGB8 128 224 224
        , PixelRGB8 192 0 0
        , PixelRGB8 192 0 32
        , PixelRGB8 192 0 64
        , PixelRGB8 192 0 96
        , PixelRGB8 192 0 128
        , PixelRGB8 192 0 160
        , PixelRGB8 192 0 192
        , PixelRGB8 192 0 224
        , PixelRGB8 192 32 0
        , PixelRGB8 192 32 32
        , PixelRGB8 192 32 64
        , PixelRGB8 192 32 96
        , PixelRGB8 192 32 128
        , PixelRGB8 192 32 160
        , PixelRGB8 192 32 192
        , PixelRGB8 192 32 224
        , PixelRGB8 192 64 0
        , PixelRGB8 192 64 32
        , PixelRGB8 192 64 64
        , PixelRGB8 192 64 96
        , PixelRGB8 192 64 128
        , PixelRGB8 192 64 160
        , PixelRGB8 192 64 192
        , PixelRGB8 192 64 224
        , PixelRGB8 192 96 0
        , PixelRGB8 192 96 32
        , PixelRGB8 192 96 64
        , PixelRGB8 192 96 96
        , PixelRGB8 192 96 128
        , PixelRGB8 192 96 160
        , PixelRGB8 192 96 192
        , PixelRGB8 192 96 224
        , PixelRGB8 192 128 0
        , PixelRGB8 192 128 32
        , PixelRGB8 192 128 64
        , PixelRGB8 192 128 96
        , PixelRGB8 192 128 128
        , PixelRGB8 192 128 160
        , PixelRGB8 192 128 192
        , PixelRGB8 192 128 224
        , PixelRGB8 192 160 0
        , PixelRGB8 192 160 32
        , PixelRGB8 192 160 64
        , PixelRGB8 192 160 96
        , PixelRGB8 192 160 128
        , PixelRGB8 192 160 160
        , PixelRGB8 192 160 192
        , PixelRGB8 192 160 224
        , PixelRGB8 192 192 0
        , PixelRGB8 192 192 32
        , PixelRGB8 192 192 64
        , PixelRGB8 192 192 96
        , PixelRGB8 192 192 128
        , PixelRGB8 192 192 160
        , PixelRGB8 240 251 255
        , PixelRGB8 164 160 160
        , PixelRGB8 128 128 128
        , PixelRGB8 0 0 255
        , PixelRGB8 0 255 0
        , PixelRGB8 0 255 255
        , PixelRGB8 255 0 0
        , PixelRGB8 255 0 255
        , PixelRGB8 255 255 0
        , PixelRGB8 255 255 255
        ]

main :: IO ()
main = putStrLn "hello world"
