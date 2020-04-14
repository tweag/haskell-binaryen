-- | Globals.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Global where

import Binaryen.Type
import Binaryen.Expression
import Foreign (Ptr, Storable)
import Foreign.C (CChar(..), CInt(..))

newtype Global = Global (Ptr Global)
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenGlobalGetName"
  getName ::
    Global -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalGetType"
  getType ::
    Global -> IO Type

foreign import ccall unsafe "BinaryenGlobalIsMutable"
  isMutable ::
    Global -> IO CInt

foreign import ccall unsafe "BinaryenGlobalGetInitExpr"
  getInitExpr ::
    Global -> IO Expression

foreign import ccall unsafe "BinaryenGlobalImportGetModule"
  importGetModule ::
    Global -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalImportGetBase"
  globalImportGetBase ::
    Global -> IO (Ptr CChar)
