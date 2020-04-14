-- | WebAssembly features.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Features where

import Data.Bits (Bits)
import Data.Word (Word32)
import Foreign (Storable)

newtype Features = Features Word32
  deriving (Bits, Eq, Show, Storable)

foreign import ccall unsafe "BinaryenFeatureMVP" mvp :: Features
foreign import ccall unsafe "BinaryenFeatureAtomics" atomics :: Features
foreign import ccall unsafe "BinaryenFeatureBulkMemory" bulkMemory :: Features
foreign import ccall unsafe "BinaryenFeatureMutableGlobals" mutableGlobals :: Features
foreign import ccall unsafe "BinaryenFeatureNontrappingFPToInt" nontrappingFPToInt :: Features
foreign import ccall unsafe "BinaryenFeatureSignExt" signExt :: Features
foreign import ccall unsafe "BinaryenFeatureSIMD128" simd128 :: Features
foreign import ccall unsafe "BinaryenFeatureExceptionHandling" exceptionHandling :: Features
foreign import ccall unsafe "BinaryenFeatureTailCall" tailCall :: Features
foreign import ccall unsafe "BinaryenFeatureReferenceTypes" referenceTypes :: Features
foreign import ccall unsafe "BinaryenFeatureAll" all :: Features
