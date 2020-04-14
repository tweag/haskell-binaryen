-- | Core types.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Type where

import Data.Word (Word32)
import Foreign (Ptr, Storable)

newtype Type = Type Word32
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenTypeNone" none :: Type
foreign import ccall unsafe "BinaryenTypeInt32" int32 :: Type
foreign import ccall unsafe "BinaryenTypeInt64" int64 :: Type
foreign import ccall unsafe "BinaryenTypeFloat32" float32 :: Type
foreign import ccall unsafe "BinaryenTypeFloat64" float64 :: Type
foreign import ccall unsafe "BinaryenTypeVec128" vec128 :: Type
foreign import ccall unsafe "BinaryenTypeFuncref" funcref :: Type
foreign import ccall unsafe "BinaryenTypeAnyref" anyref :: Type
foreign import ccall unsafe "BinaryenTypeNullref" nullref :: Type
foreign import ccall unsafe "BinaryenTypeExnref" exnref :: Type
foreign import ccall unsafe "BinaryenTypeUnreachable" unreachable :: Type
foreign import ccall unsafe "BinaryenTypeAuto" auto :: Type

foreign import ccall unsafe "BinaryenTypeCreate"
  create :: Ptr Type -> Word32 -> IO Type

foreign import ccall unsafe "BinaryenTypeArity"
  arity :: Type -> IO Word32

foreign import ccall unsafe "BinaryenTypeExpand"
  expand :: Type -> Ptr Type -> IO ()
