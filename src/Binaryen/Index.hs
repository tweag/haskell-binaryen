{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Binaryen index type.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.
module Binaryen.Index where

import Data.Word (Word32)
import Foreign (Storable)

newtype Index = Index Word32
  deriving newtype
    ( Enum,
      Eq,
      Integral,
      Num,
      Ord,
      Real,
      Show,
      Storable
    )
