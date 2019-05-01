{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude((-), (*), (+), compare)
import Control.Monad hiding (replicateM)
import Control.Applicative
import Codec.Picture
import Codec.Picture.Png
import Data.ByteString hiding (putStrLn, zipWith, head, maximum)
import Data.ByteString.Lazy (toStrict)
import Data.Serialize.Get
import Data.Functor
import Data.Function
import Data.Maybe
import Data.Vector hiding (drop, take)
import Text.Show (Show)
import Data.Int
import Data.Word (Word16, Word32, Word8)
import Prelude (undefined, fromIntegral)
import System.IO (IO, putStrLn)


-- 16 Bytes
data Tile = Tile
    { width :: Word32
    , height :: Word32
    , offset :: Word32
    , x :: Word32
    , y :: Word32
    , extraType :: Word32
    , extraRows :: Word32
    , unknowenByte1 :: Word32
    , unknowenByte2 :: Word32
    }
  deriving (Show)

data TileWithData = TileWithData
    { tile :: Tile
    , indicesToPallet :: Vector Word8
    }
  deriving (Show)

-- 8 Bytes
data File = File
    { unknowenByte3 :: Word16
    , numberOfTiles :: Word16
    , unknowenByte4 :: Word32
    , tiles :: Vector TileWithData
    }
  deriving (Show)

getFile :: Get File
getFile = do
    unknowenByte3' <- getWord16le
    numberOfTiles' <- getWord16le
    unknowenByte4' <- getWord32le
    tiles' <- replicateM (fromIntegral numberOfTiles') getTile
    dataStartPosition <- bytesRead
    data' <- remaining >>= getByteString

    let zipedTiles = zipWith TileWithData tiles'
            $ fmap (getTileData dataStartPosition data') tiles'

    pure $ File unknowenByte3' numberOfTiles' unknowenByte4' zipedTiles
  where
    getTileData :: Int -> ByteString -> Tile -> Vector Word8
    getTileData dataStartPosition data' Tile{..} =
        fromList . unpack . take ((fromIntegral height) * (fromIntegral width))
        $ drop (fromIntegral offset - dataStartPosition) data'


getTile :: Get Tile
getTile = Tile
    <$> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> getWord32le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord8
    <*> fmap fromIntegral getWord8
    <*> fmap fromIntegral getWord8
    <*> fmap fromIntegral getWord8

getSize :: Vector TileWithData -> (Word32, Word32)
getSize tiles =
    ( maximum $ fmap (\(TileWithData Tile{..} _) -> x + width) tiles
    , maximum $ fmap (\(TileWithData Tile{..} _) -> y + height) tiles
    )

magic :: File -> IO ()
magic File{..} = writeFile "asdf.png" . toStrict . encodePng $ generateImage
    (\x y -> fromMaybe 0 $ data' !? (y * (fromIntegral width) + x))
    (fromIntegral width)
    (fromIntegral height)
  where
    TileWithData Tile{..} data' = head tiles

main :: IO ()
main = putStrLn "hello world"
