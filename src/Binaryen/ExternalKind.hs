-- | External kinds.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.ExternalKind where

import Data.Word (Word32)
import Foreign (Storable)

newtype ExternalKind = ExternalKind Word32
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenExternalFunction" externalFunction :: ExternalKind
foreign import ccall unsafe "BinaryenExternalTable" externalTable :: ExternalKind
foreign import ccall unsafe "BinaryenExternalMemory" externalMemory :: ExternalKind
foreign import ccall unsafe "BinaryenExternalGlobal" externalGlobal :: ExternalKind
foreign import ccall unsafe "BinaryenExternalEvent" externalEvent :: ExternalKind
