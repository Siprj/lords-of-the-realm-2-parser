{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords.PalletMap
    ( PalletMap
    , createPalletMap
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), mapM)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (zip, nub)
import Data.Map (Map, fromList)
import System.FilePath ((</>))
import System.IO (IO, FilePath)

import Lords.Duo (Duo(palletFile), PalletFileName, DuoList)
import Lords.Pallet.Parser (parsePallet)
import Lords.Pallet.Types (Pallet)
import Utils (eitherToError)


type PalletMap = Map PalletFileName Pallet

toUniquePalletNames :: DuoList -> [PalletFileName]
toUniquePalletNames = nub . fmap palletFile

-- TODO: this should probably return either
createPalletMap :: DuoList -> FilePath -> IO PalletMap
createPalletMap duoList assetRoot = do
    pallets <- mapM (\p -> parsePallet (assetRoot </> p) >>= eitherToError)
        uniqueDuoList
    pure . fromList $ zip uniqueDuoList pallets
  where
    uniqueDuoList = toUniquePalletNames duoList

