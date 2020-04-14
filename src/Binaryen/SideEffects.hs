-- | Effect analysis.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.SideEffects where

import Binaryen.Expression
import Binaryen.Features
import Data.Word (Word32)
import Foreign (Storable)

newtype SideEffects = SideEffects Word32
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenSideEffectNone" none :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectBranches" branches :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectCalls" calls :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectReadsLocal" readsLocal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectWritesLocal" writesLocal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectReadsGlobal" readsGlobal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectWritesGlobal" writesGlobal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectReadsMemory" readsMemory :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectWritesMemory" writesMemory :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectImplicitTrap" implicitTrap :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectIsAtomic" isAtomic :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectThrows" throws :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectAny" any :: SideEffects

foreign import ccall unsafe "BinaryenExpressionGetSideEffects"
  getSideEffects ::
    Expression -> Features -> IO SideEffects
