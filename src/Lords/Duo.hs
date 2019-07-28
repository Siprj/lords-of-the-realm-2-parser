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
import Data.ByteString.Lazy (readFile)
import Data.Either (Either)
import Data.Functor ((<$>))
import Data.String (String)
import System.IO (IO, FilePath)
import Text.Show (Show)


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
parseDuo fileName = eitherDecode <$> readFile fileName

