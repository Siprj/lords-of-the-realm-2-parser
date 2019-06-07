{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords.Pallet.Parser
    ( getPallet
    )
  where

import Control.Applicative ((<*>), pure)
import Data.ByteString (readFile)
import Data.Either (Either)
import Data.Functor ((<$>), fmap)
import Data.Persist (Get, get, runGet)
import Data.String (String)
import Data.Vector.Unboxed (replicateM)
import Prelude ((*))
import System.IO (IO, FilePath)

import Lords.Pallet.Types (Pallet, PixelRGBA8(PixelRGBA8))


{-# INLINE getPallet #-}
getPallet :: Get Pallet
getPallet = replicateM 256 getPixel
  where
    getPixel = PixelRGBA8
        <$> fmap (4*) get
        <*> fmap (4*) get
        <*> fmap (4*) get
        <*> pure 0

{-# INLINE parsePallet #-}
parsePallet :: FilePath -> IO (Either String Pallet)
parsePallet file = runGet getPallet <$> readFile file

