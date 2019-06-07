{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords.PL8.Types
    ( PL8(..)
    , Tile(..)
    , tileHeaderToTile
    )
  where

import Data.Functor.Identity (Identity(Identity))
import Data.Int (Int)
import Data.Proxy (Proxy)
import Data.Vector.Unboxed (Vector)
import Data.Word (Word32, Word16, Word8)
import Text.Show (Show)


data Tile a = Tile
    { width :: Int
    , height :: Int
    , offset :: Int
    , x :: Int
    , y :: Int
    , extraType :: Int
    , extraRows :: Int
    , indicesToPallet :: a (Vector Word8)
    }

deriving instance (Show (a (Vector Word8))) => Show (Tile a)

{-# INLINE tileHeaderToTile #-}
tileHeaderToTile :: Tile Proxy -> Vector Word8 -> Tile Identity
tileHeaderToTile Tile{..} indices = Tile
    { width
    , height
    , offset
    , x
    , y
    , extraType
    , extraRows
    , indicesToPallet = Identity indices
    }

data PL8 = PL8
    { numberOfTiles :: Word16
    , tiles :: [Tile Identity]
    }
  deriving (Show)
