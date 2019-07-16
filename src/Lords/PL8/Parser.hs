{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
module Lords.PL8.Parser
    ( getPL8
    , parsePL8
    )
  where

import Control.Applicative ((<*>), (<*), pure)
import Control.Monad ((>>), replicateM)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (modify, execStateT)
import Data.Bits (testBit)
import Data.ByteString (index, readFile)
import Data.Either (Either)
import Data.Eq ((==))
import Data.Foldable (for_)
import Data.Function (($), (.), flip)
import Data.Functor ((<$>), fmap)
import Data.Functor.Identity (Identity(Identity))
import Data.Int (Int)
import Data.Monoid (mempty)
import Data.Persist (Get, get, getByteString, runGet)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Traversable (mapM)
import Data.Vector.Generic (basicUnsafeFreeze)
import Data.Vector.Unboxed.Mutable (write)
import Data.Vector.Unboxed (Vector, length, replicate)
import Data.Word (Word32, Word16, Word8)
import Prelude
    ( (*)
    , (+)
    , (-)
    , (/)
    , (<)
    , (<=)
    , (>=)
    , div
    , fromIntegral
    , otherwise
    )
import System.IO (IO, FilePath)
import qualified Control.Monad.Trans.State.Strict as S (get)
import qualified Data.Vector.Unboxed as VU (replicateM)
import qualified Data.Vector.Unboxed.Mutable as VM (replicate)

import Lords.PL8.Types
    ( PL8(PL8)
    , Tile
        ( Tile
        , extraRows
        , extraType
        , height
        , indicesToPallet
        , offset
        , width
        , x
        , y
        )
    , tileHeaderToTile
    )


{-# INLINE getWord8 #-}
getWord8 :: Get Word8
getWord8 = get

{-# INLINE getWord16le #-}
getWord16le :: Get Word16
getWord16le = get

{-# INLINE getWord32le #-}
getWord32le :: Get Word32
getWord32le = get

{-# INLINE getPL8 #-}
getPL8 :: Get PL8
getPL8 = do
    rleFlag <- getWord16le
    numberOfTiles' <- getWord16le
    _ <- getWord32le
    tiles' <- replicateM (fromIntegral numberOfTiles') getTileHeader
    finalTiles <- mapM (if testBit rleFlag 0 then getRLETile else getTile) tiles'
    pure $ PL8 numberOfTiles' finalTiles

{-# INLINE getTile #-}
getTile :: Tile Proxy -> Get (Tile Identity)
getTile header@Tile{..} =
    if extraType == 0
       then getSimplTile header
       else getISOTile header

{-# INLINE getSimplTile #-}
getSimplTile :: Tile Proxy -> Get (Tile Identity)
getSimplTile header@Tile{..} =
    tileHeaderToTile header <$> VU.replicateM (height * width) getWord8

{-# INLINE getTileHeader #-}
getTileHeader :: Get (Tile Proxy)
getTileHeader = fmap correctExtraRows $ Tile
    <$> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord32le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord16le
    <*> fmap fromIntegral getWord8
    <*> (fmap fromIntegral getWord8 <* getWord8 <* getWord8)
    <*> pure Proxy
  where
    correctExtraRows tile@Tile{..} = tile
        { extraRows = if extraType == 1 then 0 else extraRows
        }

{-# INLINE getRLETile #-}
getRLETile :: Tile Proxy -> Get (Tile Identity)
getRLETile header@Tile{..} =
    tileHeaderToTile header <$> getRLEData (fromIntegral (height * width))

{-# INLINE getRLEData #-}
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
                data' <- VU.replicateM numOfOpaquePixels getWord8
                go $ pixels <> data'

{-# INLINE getISOTile #-}
getISOTile :: Tile Proxy -> Get (Tile Identity)
getISOTile header@Tile{..} = do
    data' <- getByteString $ dataLength + extraRows
        * (rightOffset - leftOffset)
    let magic = tileHeaderToTile header (indices data')
    pure $ magic { height = height + extraRows
                 , y = y - extraRows
                 }
  where
    indices data' = runST $ do
        indices' <- VM.replicate ((height + extraRows)* width) 0
        flip execStateT 0
            ( fillTopHalf data' indices'
            >> fillBottomHalf data' indices'
            >> fillExtracs data' indices'
            )
        basicUnsafeFreeze indices'

    fillTopHalf data' indices' = do
        for_ [0 .. halfHeight - 1] $ \y ->
            for_ [firstHalfRowStart y .. firstHalfRowStop y - 1] $ \x -> (do
                dataIndex <- S.get
                lift $ write indices' ((y + extraRows) * width + x) (data' `index` dataIndex)
                modify (+1))

    fillBottomHalf data' indices' = do
        for_ [halfHeight .. height - 1] $ \y ->
            for_ [secondHalfRowStart y .. secondHalfRowStop y - 1] $ \x -> (do
                dataIndex <- S.get
                lift $ write indices' ((y + extraRows) * width + x) (data' `index` dataIndex)
                modify (+1)
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
                   else lift $ write indices' (yMod * width + x) (data' `index` dataIndex)
                modify (+1)
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

-- | Count down
{-# INLINE dn #-}
dn :: Int -> Int -> [Int]
dn x y
    | x <= y = []
    | otherwise = go_dn x
  where
    go_dn x'
        | x' <= y = []
        | otherwise = x' : go_dn (x'-1)

-- | Count up
{-# INLINE up #-}
up :: Int -> Int -> [Int]
up x y
    | x >= y = []
    | otherwise = go_up x
  where
    go_up x'
        | x' >= y = []
        | otherwise = x' : go_up (x'+1)

parsePL8 :: FilePath -> IO (Either String PL8)
parsePL8 file = runGet getPL8 <$> readFile file
