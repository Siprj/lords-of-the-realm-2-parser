{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Codec.Picture.Png
import Codec.Picture.Types
import Control.Applicative
import Control.Monad hiding (replicateM)
import qualified Control.Monad as M (replicateM)
import Control.Monad.ST
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Class
import Data.ByteString hiding (putStrLn, zipWith, head, maximum, zip, replicate, length, take)
import qualified Data.ByteString as BS
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
import Data.Persist
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Vector hiding (drop, take, maximum, mapM_, mapM, sequence, length)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Generic as V
import Data.Word (Word16, Word32, Word8)
import Debug.Trace
import Numeric
import qualified Data.List as L
import Prelude((-), (*), (/), div, (+), (<), (>=), (==), (/=), (<=),  (||), compare, uncurry, undefined, fromIntegral, min, take)
import qualified Prelude as P
import System.FilePath
import System.FilePath.Glob
import System.IO (IO, putStrLn, print, FilePath, Handle)
import Text.Show (Show, show)
import Text.Printf (printf, hPrintf)


data Tile a = Tile
    { width :: Int
    , height :: Int
    , offset :: Int
    , x :: Int
    , y :: Int
    , extraType :: Word32
    , extraRows :: Int
    , unknowenByte1 :: Word8
    , unknowenByte2 :: Word8
    , indicesToPallet :: a (VU.Vector Word8)
    }

getWord8 :: Get Word8
getWord8 = get

getWord16le :: Get Word16
getWord16le = get

getWord32le :: Get Word32
getWord32le = get

{-# INLINE tileHeaderToTile #-}
tileHeaderToTile :: Tile Proxy -> VU.Vector Word8 -> Tile Identity
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

deriving instance (Show (a (VU.Vector Word8))) => Show (Tile a)

data File = File
    { unknowenByte3 :: Word16
    , numberOfTiles :: Word16
    , unknowenByte4 :: Word32
    , tiles :: [Tile Identity]
    }
  deriving (Show)

getWord8B :: ByteString -> (Word8, ByteString)
getWord8B d = let (Just v) = BS.uncons d in v

getWord16B :: ByteString -> (Word16, ByteString)
getWord16B d =
    let (w1, d1) = getWord8B d in
    let (w2, d2) = getWord8B d1 in
    (fromIntegral w1 .|. (fromIntegral w2) `shift` 8, d2)

getWord32B :: ByteString -> (Word32, ByteString)
getWord32B d =
    let (w1, d1) = getWord8B d in
    let (w2, d2) = getWord8B d1 in
    let (w3, d3) = getWord8B d2 in
    let (w4, d4) = getWord8B d3 in
    trace ("w1: " <> show w1 <> "w2: " <> show w2 <> "w3: " <> show w3 <> "w4: " <> show w4)
        $ (fromIntegral w1
        .|. (fromIntegral w2) `shift` 8
        .|. (fromIntegral w3) `shift` 16
        .|. (fromIntegral w4) `shift` 24
    , d4
    )

getRLEDataB :: ByteString -> Int -> Int -> VU.Vector Word8
getRLEDataB data' offset size = runST $ do
    indices <- VM.replicate size 0
    S.execStateT go (RleState 0 0 indices d)
    V.basicUnsafeFreeze indices
  where
    d = BS.drop offset data'

    go = do
        n <- getWriteIndex
        when (n < size) $ do
            numOfOpaquePixels <- fromIntegral <$> stateReadWord8
            if numOfOpaquePixels == 0
                then do
                    numberOfTransparetnPixels <- fromIntegral <$> stateReadWord8
                    for_ [0 .. (numberOfTransparetnPixels - 1)]
                        $ const (writePixelRle 0)
                    go
                else do
                    for_ [0 .. (numOfOpaquePixels - 1)] $ \_ -> do
                        stateReadWord8 >>= writePixelRle
                    go

addWriteIndex :: forall s. RleStateRun s ()
addWriteIndex = S.modify' (\ s@RleState{..} -> s { writeIndex = writeIndex + 1 })

addReadIndex :: forall s. RleStateRun s ()
addReadIndex = S.modify' (\ s@RleState{..} -> s { readIndex = readIndex + 1 })

getWriteIndex :: forall s. RleStateRun s Int
getWriteIndex = S.gets writeIndex

writePixelRle :: forall s. Word8 -> RleStateRun s ()
writePixelRle w = do
    RleState{..} <- S.get
    lift $ VM.write indices writeIndex w
    addWriteIndex

stateReadWord8 :: forall s. RleStateRun s Word8
stateReadWord8 = do
    RleState{..} <- S.get
    let w = data' `index` readIndex
    addReadIndex
    pure w

data RleState s = RleState
    { readIndex :: Int
    , writeIndex :: Int
    , indices :: VM.STVector s Word8
    , data' :: ByteString
    }

type RleStateRun s = S.StateT (RleState s) (ST s)


getTileHeaderB :: ByteString -> ByteString -> (Tile Identity, ByteString)
getTileHeaderB originalData d =
    let (width, d1) = getWord16B d in
    let (height, d2) = getWord16B d1 in
    let (offset', d3) = getWord32B d2 in
    let (x, d4) = getWord16B d3 in
    let (y, d5) = getWord16B d4 in
    let (extraType, d6) = getWord8B d5 in
    let (extraRows, d7) = getWord8B d6 in
    ( Tile
        { width = fromIntegral width
        , height = fromIntegral height
        , offset = fromIntegral offset'
        , x = fromIntegral x
        , y = fromIntegral y
        , extraType = fromIntegral extraType
        , extraRows = fromIntegral extraRows
        , unknowenByte1 = 0
        , unknowenByte2 = 0
        -- TODO: Create function wich will extract indices form data.
        -- TODO: Add parameter with "global" ByteString so we can use it to
        -- reference particular offset.
        , indicesToPallet = Identity $ getRLEDataB
            originalData
            (fromIntegral offset')
            (fromIntegral width * fromIntegral height)
--        , indicesToPallet = Identity mempty
        }
    , drop 2 d7
    )


getFileB :: ByteString -> File
getFileB d = File 0 numOfTilesB 0 . fst $ getTileHeaders (fromIntegral numOfTilesB) d2
  where
    -- Should I use unsafe drop???
    (numOfTilesB, d1) = getWord16B $ BS.drop 2 d
    d2 = BS.drop 4 d1

    getTileHeaders :: Int -> ByteString -> ([Tile Identity], ByteString)
    getTileHeaders n d'
        | n P.> 0 =
            let (tile, d2') = getTileHeaderB d d' in
            let (tiles, dlast) = getTileHeaders (n - 1) d2' in
            (tile : tiles, dlast)
        | P.otherwise = ([], d')

{-# INLINE getPallet #-}
getPallet :: Get (Vector PixelRGB8)
getPallet = replicateM 256 getPixel
  where
    getPixel = PixelRGB8
        <$> fmap (4*) getWord8
        <*> fmap (4*) getWord8
        <*> fmap (4*) getWord8

{-# INLINE printFileHeaders #-}
printFileHeaders :: Handle -> IO ()
printFileHeaders fHandle = do
    files <- L.sort <$> globDir1 (compile "*.pl8") "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/"
    for_ files $ \f -> do
        putStrLn f
        parsedFile <- fmap (fmap (magic f) . (runGet getFile)) $ readFile f
        either putStrLn printHeader parsedFile
  where
    printHeader :: (File, FilePath) -> IO ()
    printHeader (File{..}, filePath) = do
        hPrintf fHandle "0x%08x 0x%04x %s\n" unknowenByte4 unknowenByte3 filePath
        mapM_ printTileHeader tiles

    magic :: FilePath -> File -> (File, FilePath)
    magic filePath file = (file, filePath)

    printTileHeader :: Tile a -> IO ()
    printTileHeader Tile{..} =
        if extraType /= 0
            then do
                hPrintf fHandle "    extra type: 0x%08x extraRows: %d" extraType extraRows
                hPrintf fHandle " offset: %d width: %d height: %d\n" offset width height
            else pure ()

{-# INLINE convertFiles #-}
convertFiles :: IO ()
convertFiles = do
    files <- L.sort <$> globDir1 (compile "A2*.pl8") "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/"
    let inOutFiles = P.zip files $ fmap
            ((flip replaceDirectory) "/home/yrid/pokus2/"
            . (flip replaceExtensions) "png") files
    mapM_ convert inOutFiles
  where
    convert (input, output) =
        convertToRgb input output "/home/yrid/.local/share/Steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/Base01.256"

{-# INLINE eitherToError #-}
eitherToError :: Either String b -> IO b
eitherToError (Right a) = pure a
eitherToError (Left e) = fail e

{-# INLINE convertBy #-}
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

{-# INLINE convertToRgb #-}
convertToRgb :: FilePath -> FilePath -> FilePath -> IO ()
convertToRgb input output palletFile = do
    pallet <- readPallet
    convertBy (fileToRGBImage pallet) input output
  where
    readPallet = readFile palletFile >>= (eitherToError . runGet getPallet)

getFile :: Get File
getFile = do
    unknowenByte3' <- getWord16le
    numberOfTiles' <- getWord16le
    unknowenByte4' <- getWord32le
    tiles' <- M.replicateM (fromIntegral numberOfTiles') getTileHeader
    finalTiles <- mapM (if testBit unknowenByte3' 0 then getRLETile else getTile) tiles'
    pure $ File unknowenByte3' numberOfTiles' unknowenByte4' finalTiles

{-# INLINE getTile #-}
getTile :: Tile Proxy -> Get (Tile Identity)
getTile header@Tile{..} = do
    if extraType == 0
       then getSimplTile header
       else getISOTile header

{-# INLINE getISOTile #-}
getISOTile :: Tile Proxy -> Get (Tile Identity)
getISOTile header@Tile{..} = do
    data' <- getByteString $ dataLength + (fromIntegral extraRows)
        * (rightOffset - leftOffset)
    let magic = tileHeaderToTile header (indices data')
    pure $ magic { height = height + extraRows
                 , y = y - extraRows
                 }
  where
    indices data' = runST $ do
        indices' <- VM.replicate ((height + extraRows)* width) 0
        flip S.execStateT 0
            ( fillTopHalf data' indices'
            >> fillBottomHalf data' indices'
            >> fillExtracs data' indices'
            )
        V.basicUnsafeFreeze indices'

    fillTopHalf data' indices' = do
        for_ [0 .. halfHeight - 1] $ \y ->
            for_ [firstHalfRowStart y .. firstHalfRowStop y - 1] $ \x -> (do
                dataIndex <- S.get
                lift $ VM.write indices' ((y + extraRows) * width + x) (data' `index` dataIndex)
                S.modify (+1))

    fillBottomHalf data' indices' = do
        for_ [halfHeight .. height - 1] $ \y ->
            for_ [secondHalfRowStart y .. secondHalfRowStop y - 1] $ \x -> (do
                dataIndex <- S.get
                lift $ VM.write indices' ((y + extraRows) * width + x) (data' `index` dataIndex)
                S.modify (+1)
                )

    fillExtracs data' indices' = do
        for_ (dn extraRows 0) $ \y ->
            for_ (up leftOffset rightOffset) $ \x -> (do
                let yMod = if x <= halfWidth
                        then y + (halfHeight - 1) - (x `div` 2)
                        else y + (x `div` 2) - (halfHeight - 1)
                dataIndex <- S.get
                if (data' `index` dataIndex) == 0
                   then pure ()
                   else lift $ VM.write indices' (yMod * width + x) (data' `index` dataIndex)
                S.modify (+1)
                )

    rightOffset :: Int
    rightOffset = if extraType == 3 then halfWidth + 1 else width
    leftOffset :: Int
    leftOffset = if extraType == 4 then halfWidth - 1 else 0
    halfWidth :: Int
    halfWidth = width `div` 2
    halfHeight :: Int
    halfHeight = height `div` 2
    firstHalfRowStart :: Int -> Int
    firstHalfRowStart y = (halfHeight - 1 - y) * 2
    firstHalfRowStop :: Int -> Int
    firstHalfRowStop y = firstHalfRowStart y + (y * 4) + 2
    secondHalfRowStart :: Int -> Int
    secondHalfRowStart y = (halfHeight - 1 - (height - y - 1)) * 2
    secondHalfRowStop :: Int -> Int
    secondHalfRowStop y = secondHalfRowStart y + ((height - y -1) * 4) + 2
    -- This is sum of arithmetic progression. Division by two is not used
    -- because we need two half of the image.
    dataLength = halfHeight * (2 + ((halfHeight - 1) * 4) + 2)
    --                         ^   ^                        ^
    --          first element -/   \<----- nth element ---->/

{-# INLINE dn #-}
dn :: Int -> Int -> [Int]
dn x y
    | (x <= y) = []
    | P.otherwise = go_dn x
  where
    go_dn x'
        | x' <= y = []
        | P.otherwise = x' : go_dn (x'-1)

{-# INLINE up #-}
up :: Int -> Int -> [Int]
up x y
    | (x >= y) = []
    | P.otherwise = go_up x
  where
    go_up x'
        | x' >= y = []
        | P.otherwise = x' : go_up (x'+1)



{-# INLINE getSimplTile #-}
getSimplTile :: Tile Proxy -> Get (Tile Identity)
getSimplTile header@Tile{..} = do
    tileHeaderToTile header <$> VU.replicateM (height * width) getWord8

{-# INLINE getRLETile #-}
getRLETile :: Tile Proxy -> Get (Tile Identity)
getRLETile header@Tile{..} =
    tileHeaderToTile header <$> getRLEData (fromIntegral (height * width))

{-# INLINE getRLEData #-}
getRLEData :: Int -> Get (VU.Vector Word8)
getRLEData size = go mempty
  where
    go :: VU.Vector Word8 -> Get (VU.Vector Word8)
    go pixels = if VU.length pixels < size
        then readData pixels
        else pure pixels

    readData pixels = do
        numOfOpaquePixels <- fromIntegral <$> getWord8
        if numOfOpaquePixels == 0
            then do
                numberOfTransparetnPixels <- fromIntegral <$> getWord8
                go $ pixels <> VU.replicate numberOfTransparetnPixels 0
            else do
                data' <- VU.replicateM numOfOpaquePixels getWord8
                go $ pixels <> data'

{-# INLINE getTileHeader #-}
getTileHeader :: Get (Tile Proxy)
getTileHeader = fmap correctExtraRows $ Tile
    <$> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord32le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord8
    <*> fmap fromIntegral getWord8
    <*> getWord8
    <*> getWord8
    <*> pure Proxy
  where
    correctExtraRows tile@Tile{..} = tile
        { extraRows = if extraType == 1 then 0 else extraRows
        }

{-# INLINE getSize #-}
getSize :: [Tile a] -> (Int, Int)
getSize tiles =
    ( maximum $ fmap (\Tile{..} -> x + width) tiles
    , maximum $ fmap (\Tile{..} -> y + height) tiles
    )

fileToRGBImage :: Vector PixelRGB8 -> File -> IO (Image PixelRGB8)
fileToRGBImage pallet File{..} = do
    let (width, height) = getSize tiles
    image <- newMutableImage @PixelRGB8 width height
    mapM_ (fillPixels image) tiles
    unsafeFreezeImage image
  where
    makeCoordinates :: Tile Identity -> [(Int, Int)]
    makeCoordinates Tile{..} =
        [(x, y) | y <- [y..y+height-1], x <- [x..x+width-1]]

    fillPixels :: MutableImage RealWorld PixelRGB8 -> Tile Identity -> IO ()
    fillPixels image tile@Tile{..} =
        fillPixels' image (runIdentity indicesToPallet) (makeCoordinates tile)

    fillPixels'
        :: MutableImage RealWorld PixelRGB8
        -> VU.Vector Word8
        -> [(Int, Int)]
        -> IO ()
    fillPixels' image imageData ((x, y) : coordinates) = do
        let (pixel, imageData') = uncons imageData
        writePixel image x y $ pallet ! fromIntegral pixel
        fillPixels' image imageData' coordinates
    fillPixels' _ _ [] = pure ()

    uncons vec = let (v, vs) = VU.splitAt 1 vec in (VU.head v, vs)

main :: IO ()
main = convertFiles
