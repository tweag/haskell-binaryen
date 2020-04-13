-- | Exports.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Export where

import Binaryen.ExternalKind
import Foreign (Ptr, Storable)
import Foreign.C (CChar)

newtype Export = Export (Ptr Export)
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenExportGetKind"
  getKind ::
    Export -> IO ExternalKind

foreign import ccall unsafe "BinaryenExportGetName"
  getName ::
    Export -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenExportGetValue"
  getValue ::
    Export -> IO (Ptr CChar)
