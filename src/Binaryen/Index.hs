-- | The Binaryen index type.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Index where

import Data.Word (Word32)
import Foreign (Storable)

newtype Index = Index Word32
  deriving newtype (Eq, Num, Show, Storable)
