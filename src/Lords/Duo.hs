{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lords.Duo
    ( Duo(..)
    , DuoList
    , PalletFileName
    , PL8FileName
    , parseDuo
    )
  where

import Data.Aeson (eitherDecode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Functor ((<$>))
import Data.ByteString.Lazy (readFile)
import Data.Map.Strict (Map)
import Data.String (String)
import Data.Either (Either)
import Text.Show (Show)
import System.IO (IO, FilePath)


type PalletFileName = String
type PL8FileName = String

data Duo = Duo
    { palletFile :: PalletFileName
    , pl8File :: PL8FileName
    }
  deriving (Show)

type DuoList = [Duo]

$(deriveJSON defaultOptions ''Duo)

parseDuo :: FilePath -> IO (Either String DuoList)
parseDuo fileName = do
    eitherDecode <$> readFile fileName

