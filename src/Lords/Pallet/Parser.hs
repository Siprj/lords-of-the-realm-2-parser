{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords.Pallet.Parser
    ( getPallet
    , parsePallet
    )
  where

import Control.Applicative ((<*>), pure)
import Data.ByteString (readFile)
import Data.Either (Either)
import Data.Functor ((<$>), ($>), fmap)
import Data.Persist (Get, get, runGet)
import Data.String (String)
import Data.Vector.Unboxed (generateM)
import Prelude ((*), (==), otherwise)
import System.IO (IO, FilePath)

import Lords.Pallet.Types (Pallet, PixelRGBA8(PixelRGBA8))


{-# INLINE getPallet #-}
getPallet :: Get Pallet
getPallet =
    generateM 256 getPixel
  where
    getPixel' = PixelRGBA8
        <$> fmap (4*) get
        <*> fmap (4*) get
        <*> fmap (4*) get
        <*> pure 255

    getPixel i
        | i == 0 = getPixel' $> PixelRGBA8 0 0 0 0
        | otherwise = getPixel'

{-# INLINE parsePallet #-}
parsePallet :: FilePath -> IO (Either String Pallet)
parsePallet file = runGet getPallet <$> readFile file

