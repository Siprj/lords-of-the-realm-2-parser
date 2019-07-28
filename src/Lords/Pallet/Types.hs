{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module Lords.Pallet.Types
    ( PixelRGBA8(..)
    , Pallet
    )
  where

import Control.DeepSeq (NFData)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Vector.Unboxed (Vector, MVector, Unbox)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Prelude (undefined)

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M


data PixelRGBA8 = PixelRGBA8
    { r :: Word8
    , g :: Word8
    , b :: Word8
    , a :: Word8
    }
  deriving (Generic, NFData)

data instance MVector s PixelRGBA8
    = MV_PixelRGBA8 (MVector s (Word8, Word8, Word8, Word8))

data instance Vector PixelRGBA8
    = V_PixelRGBA8 (Vector (Word8, Word8, Word8, Word8))

instance Unbox PixelRGBA8

{-# INLINE packPixelRGBA8 #-}
packPixelRGBA8 :: PixelRGBA8 -> (Word8, Word8, Word8, Word8)
packPixelRGBA8 PixelRGBA8{..} = (r, g, b, a)

{-# INLINE unpackPixelRGBA8 #-}
unpackPixelRGBA8 :: (Word8, Word8, Word8, Word8) -> PixelRGBA8
unpackPixelRGBA8 (r, g, b, a) = PixelRGBA8{r, g, b, a}

instance M.MVector MVector PixelRGBA8 where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_PixelRGBA8 v) = M.basicLength v
    basicUnsafeSlice i n (MV_PixelRGBA8 v) =
        MV_PixelRGBA8 $ M.basicUnsafeSlice i n v
    basicOverlaps (MV_PixelRGBA8 v1) (MV_PixelRGBA8 v2) = M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_PixelRGBA8 <$> M.basicUnsafeNew n
    basicInitialize (MV_PixelRGBA8 v) = M.basicInitialize v
    basicUnsafeReplicate n pixel =
        MV_PixelRGBA8 <$> M.basicUnsafeReplicate n (packPixelRGBA8 pixel)
    basicUnsafeRead (MV_PixelRGBA8 v) i =
        unpackPixelRGBA8 <$> M.basicUnsafeRead v i
    basicUnsafeWrite (MV_PixelRGBA8 v) i pixel =
        M.basicUnsafeWrite v i $ packPixelRGBA8 pixel
    basicClear (MV_PixelRGBA8 v) = M.basicClear v
    basicSet (MV_PixelRGBA8 v) pixel = M.basicSet v $ packPixelRGBA8 pixel
    basicUnsafeCopy (MV_PixelRGBA8 v1) (MV_PixelRGBA8 v2) =
        M.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_PixelRGBA8 v1) (MV_PixelRGBA8 v2) =
        M.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_PixelRGBA8 v) n =
        MV_PixelRGBA8 <$> M.basicUnsafeGrow v n

instance G.Vector Vector PixelRGBA8 where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_PixelRGBA8 v) = V_PixelRGBA8 <$> G.basicUnsafeFreeze v
    basicUnsafeThaw (V_PixelRGBA8 v) = MV_PixelRGBA8 <$> G.basicUnsafeThaw v
    basicLength (V_PixelRGBA8 v) = G.basicLength v
    basicUnsafeSlice i n (V_PixelRGBA8 v) =
        V_PixelRGBA8 $ G.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_PixelRGBA8 v) i
                  = unpackPixelRGBA8 <$> G.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_PixelRGBA8 mv) (V_PixelRGBA8 v)
                  = G.basicUnsafeCopy mv v
    elemseq _ PixelRGBA8{..} z = G.elemseq (undefined :: Vector a) a
                         . G.elemseq (undefined :: Vector a) b
                         . G.elemseq (undefined :: Vector a) g
                         $ G.elemseq (undefined :: Vector a) r z

type Pallet = Vector PixelRGBA8
