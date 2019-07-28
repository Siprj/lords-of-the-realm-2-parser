{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils
    ( eitherToError
    )
  where

import Control.Monad (fail)
import Control.Applicative (pure)
import Data.Either (Either(Right, Left))
import Data.String (String)
import System.IO (IO)


{-# INLINE eitherToError #-}
eitherToError :: Either String b -> IO b
eitherToError (Right a) = pure a
eitherToError (Left e) = fail e
