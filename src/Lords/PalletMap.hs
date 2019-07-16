{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords.PalletMap
    ( PalletMap
    , createPalletMap
    )
  where

import Data.Map (Map, fromList)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (zip, nub)
import Control.Applicative (pure)
import Control.Monad ((>>=), mapM)
import System.FilePath ((</>))
import System.IO (IO, FilePath)

import Utils (eitherToError)
import Lords.Pallet.Parser (parsePallet)
import Lords.Pallet.Types (Pallet, PixelRGBA8(..))
import Lords.PL8.Parser (parsePL8)
import Lords.PL8.Types (PL8(..), Tile(..))
import Lords.Duo (Duo(palletFile), PalletFileName, DuoList)


type PalletMap = Map PalletFileName Pallet

toUniquePalletNames :: DuoList -> [PalletFileName]
toUniquePalletNames = nub . fmap palletFile

-- TODO: this should probably return either
createPalletMap :: DuoList -> FilePath -> IO PalletMap
createPalletMap duoList assetRoot = do
    pallets <- mapM (\p -> parsePallet (assetRoot </> p) >>= eitherToError)
        $ uniqueDuoList
    pure . fromList $ zip uniqueDuoList pallets
  where
    uniqueDuoList = toUniquePalletNames duoList

