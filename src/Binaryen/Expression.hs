-- | Expressions.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Expression where

import Binaryen.ExpressionId
import Binaryen.Index
import {-# SOURCE #-} Binaryen.Module
import Binaryen.Op
import Binaryen.Type
import Data.Int (Int8, Int32, Int64)
import Data.Word (Word32, Word8)
import Foreign (Ptr, Storable)
import Foreign.C (CChar(..), CDouble(..), CFloat(..), CInt(..))

newtype Expression = Expression (Ptr Expression)
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenConstInt32"
  constInt32 ::
    Module -> Int32 -> IO Expression

foreign import ccall unsafe "BinaryenConstInt64"
  constInt64 ::
    Module -> Int64 -> IO Expression

foreign import ccall unsafe "BinaryenConstFloat32"
  constFloat32 ::
    Module -> Float -> IO Expression

foreign import ccall unsafe "BinaryenConstFloat64"
  constFloat64 ::
    Module -> Double -> IO Expression

foreign import ccall unsafe "BinaryenConstVec128"
  constVec128 ::
    Module -> Ptr Word8 -> IO Expression

foreign import ccall unsafe "BinaryenConstFloat32Bits"
  constFloat32Bits ::
    Module -> Int32 -> IO Expression

foreign import ccall unsafe "BinaryenConstFloat64Bits"
  constFloat64Bits ::
    Module -> Int64 -> IO Expression

foreign import ccall unsafe "BinaryenBlock"
  block ::
    Module ->
    Ptr CChar ->
    Ptr Expression ->
    Index ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenIf"
  if_ ::
    Module ->
    Expression ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenLoop"
  loop ::
    Module ->
    Ptr CChar ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenBreak"
  break ::
    Module ->
    Ptr CChar ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenSwitch"
  switch ::
    Module ->
    Ptr (Ptr CChar) ->
    Index ->
    Ptr CChar ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenCall"
  call ::
    Module ->
    Ptr CChar ->
    Ptr Expression ->
    Index ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenCallIndirect"
  callIndirect ::
    Module ->
    Expression ->
    Ptr Expression ->
    Index ->
    Type ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenReturnCall"
  returnCall ::
    Module ->
    Ptr CChar ->
    Ptr Expression ->
    Index ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenReturnCallIndirect"
  returnCallIndirect ::
    Module ->
    Expression ->
    Ptr Expression ->
    Index ->
    Type ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenLocalGet"
  localGet ::
    Module ->
    Index ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenLocalSet"
  localSet ::
    Module ->
    Index ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenLocalTee"
  localTee ::
    Module ->
    Index ->
    Expression ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenGlobalGet"
  globalGet ::
    Module ->
    Ptr CChar ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenGlobalSet"
  globalSet ::
    Module ->
    Ptr CChar ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenLoad"
  load ::
    Module ->
    Word32 ->
    Int8 ->
    Word32 ->
    Word32 ->
    Type ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenStore"
  store ::
    Module ->
    Word32 ->
    Word32 ->
    Word32 ->
    Expression ->
    Expression ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenUnary"
  unary ::
    Module ->
    Op ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenBinary"
  binary ::
    Module ->
    Op ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenSelect"
  select ::
    Module ->
    Expression ->
    Expression ->
    Expression ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenDrop"
  drop ::
    Module ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenReturn"
  return ::
    Module ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenHost"
  host ::
    Module ->
    Op ->
    Ptr CChar ->
    Ptr Expression ->
    Index ->
    IO Expression

foreign import ccall unsafe "BinaryenNop"
  nop ::
    Module -> IO Expression

foreign import ccall unsafe "BinaryenUnreachable"
  unreachable ::
    Module -> IO Expression

foreign import ccall unsafe "BinaryenAtomicLoad"
  atomicLoad ::
    Module ->
    Word32 ->
    Word32 ->
    Type ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenAtomicStore"
  atomicStore ::
    Module ->
    Word32 ->
    Word32 ->
    Expression ->
    Expression ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenAtomicRMW"
  atomicRMW ::
    Module ->
    Op ->
    Index ->
    Index ->
    Expression ->
    Expression ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenAtomicCmpxchg"
  atomicCmpxchg ::
    Module ->
    Index ->
    Index ->
    Expression ->
    Expression ->
    Expression ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenAtomicWait"
  atomicWait ::
    Module ->
    Expression ->
    Expression ->
    Expression ->
    Type ->
    IO Expression

foreign import ccall unsafe "BinaryenAtomicNotify"
  atomicNotify ::
    Module ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenAtomicFence"
  atomicFence ::
    Module -> IO Expression

foreign import ccall unsafe "BinaryenSIMDExtract"
  simdExtract ::
    Module ->
    Op ->
    Expression ->
    Word8 ->
    IO Expression

foreign import ccall unsafe "BinaryenSIMDReplace"
  simdReplace ::
    Module ->
    Op ->
    Expression ->
    Word8 ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenSIMDShuffle"
  simdShuffle ::
    Module ->
    Expression ->
    Expression ->
    Ptr Word8 ->
    IO Expression

foreign import ccall unsafe "BinaryenSIMDTernary"
  simdTernary ::
    Module ->
    Op ->
    Expression ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenSIMDShift"
  simdShift ::
    Module ->
    Op ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenSIMDLoad"
  simdLoad ::
    Module ->
    Op ->
    Word32 ->
    Word32 ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenMemoryInit"
  memoryInit ::
    Module ->
    Word32 ->
    Expression ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenDataDrop"
  dataDrop ::
    Module -> Word32 -> IO Expression

foreign import ccall unsafe "BinaryenMemoryCopy"
  memoryCopy ::
    Module ->
    Expression ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenMemoryFill"
  memoryFill ::
    Module ->
    Expression ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenRefNull"
  refNull ::
    Module -> IO Expression

foreign import ccall unsafe "BinaryenRefIsNull"
  refIsNull ::
    Module ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenRefFunc"
  refFunc ::
    Module -> Ptr CChar -> IO Expression

foreign import ccall unsafe "BinaryenTry"
  try ::
    Module ->
    Expression ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenThrow"
  throw ::
    Module ->
    Ptr CChar ->
    Ptr Expression ->
    Index ->
    IO Expression

foreign import ccall unsafe "BinaryenRethrow"
  rethrow ::
    Module ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenBrOnExn"
  brOnExn ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenPush"
  push ::
    Module ->
    Expression ->
    IO Expression

foreign import ccall unsafe "BinaryenPop"
  pop ::
    Module -> Type -> IO Expression

foreign import ccall unsafe "BinaryenExpressionGetId"
  getId ::
    Expression -> IO ExpressionId

foreign import ccall unsafe "BinaryenExpressionGetType"
  getType ::
    Expression -> IO Type

foreign import ccall unsafe "BinaryenExpressionPrint"
  print ::
    Expression -> IO ()

foreign import ccall unsafe "BinaryenBlockGetName"
  blockGetName ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenBlockGetNumChildren"
  blockGetNumChildren ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenBlockGetChild"
  blockGetChild ::
    Expression -> Index -> IO Expression

foreign import ccall unsafe "BinaryenIfGetCondition"
  ifGetCondition ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenIfGetIfTrue"
  ifGetIfTrue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenIfGetIfFalse"
  ifGetIfFalse ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenLoopGetName"
  loopGetName ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenLoopGetBody"
  loopGetBody ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenBreakGetName"
  breakGetName ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenBreakGetCondition"
  breakGetCondition ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenBreakGetValue"
  breakGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSwitchGetNumNames"
  switchGetNumNames ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenSwitchGetName"
  switchGetName ::
    Expression -> Index -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSwitchGetDefaultName"
  switchGetDefaultName ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSwitchGetCondition"
  switchGetCondition ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSwitchGetValue"
  switchGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenCallGetTarget"
  callGetTarget ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenCallGetNumOperands"
  callGetNumOperands ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenCallGetOperand"
  callGetOperand ::
    Expression -> Index -> IO Expression

foreign import ccall unsafe "BinaryenCallIndirectGetTarget"
  callIndirectGetTarget ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenCallIndirectGetNumOperands"
  callIndirectGetNumOperands ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenCallIndirectGetOperand"
  callIndirectGetOperand ::
    Expression -> Index -> IO Expression

foreign import ccall unsafe "BinaryenLocalGetGetIndex"
  localGetGetIndex ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenLocalSetIsTee"
  localSetIsTee ::
    Expression -> IO CInt

foreign import ccall unsafe "BinaryenLocalSetGetIndex"
  localSetGetIndex ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenLocalSetGetValue"
  localSetGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenGlobalGetGetName"
  globalGetGetName ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalSetGetName"
  globalSetGetName ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalSetGetValue"
  globalSetGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenHostGetOp"
  hostGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenHostGetNameOperand"
  hostGetNameOperand ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenHostGetNumOperands"
  hostGetNumOperands ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenHostGetOperand"
  hostGetOperand ::
    Expression -> Index -> IO Expression

foreign import ccall unsafe "BinaryenLoadIsAtomic"
  loadIsAtomic ::
    Expression -> IO CInt

foreign import ccall unsafe "BinaryenLoadIsSigned"
  loadIsSigned ::
    Expression -> IO CInt

foreign import ccall unsafe "BinaryenLoadGetOffset"
  loadGetOffset ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenLoadGetBytes"
  loadGetBytes ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenLoadGetAlign"
  loadGetAlign ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenLoadGetPtr"
  loadGetPtr ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenStoreIsAtomic"
  storeIsAtomic ::
    Expression -> IO CInt

foreign import ccall unsafe "BinaryenStoreGetBytes"
  storeGetBytes ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenStoreGetOffset"
  storeGetOffset ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenStoreGetAlign"
  storeGetAlign ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenStoreGetPtr"
  storeGetPtr ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenStoreGetValue"
  storeGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenConstGetValueI32"
  constGetValueI32 ::
    Expression -> IO Int32

foreign import ccall unsafe "BinaryenConstGetValueI64"
  constGetValueI64 ::
    Expression -> IO Int64

foreign import ccall unsafe "BinaryenConstGetValueI64Low"
  constGetValueI64Low ::
    Expression -> IO Int32

foreign import ccall unsafe "BinaryenConstGetValueI64High"
  constGetValueI64High ::
    Expression -> IO Int32

foreign import ccall unsafe "BinaryenConstGetValueF32"
  constGetValueF32 ::
    Expression -> IO CFloat

foreign import ccall unsafe "BinaryenConstGetValueF64"
  constGetValueF64 ::
    Expression -> IO CDouble

foreign import ccall unsafe "BinaryenConstGetValueV128"
  constGetValueV128 ::
    Expression -> Ptr Word8 -> IO ()

foreign import ccall unsafe "BinaryenUnaryGetOp"
  unaryGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenUnaryGetValue"
  unaryGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenBinaryGetOp"
  binaryGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenBinaryGetLeft"
  binaryGetLeft ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenBinaryGetRight"
  binaryGetRight ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSelectGetIfTrue"
  selectGetIfTrue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSelectGetIfFalse"
  selectGetIfFalse ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSelectGetCondition"
  selectGetCondition ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenDropGetValue"
  dropGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenReturnGetValue"
  returnGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicRMWGetOp"
  atomicRMWGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenAtomicRMWGetBytes"
  atomicRMWGetBytes ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenAtomicRMWGetOffset"
  atomicRMWGetOffset ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenAtomicRMWGetPtr"
  atomicRMWGetPtr ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicRMWGetValue"
  atomicRMWGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetBytes"
  atomicCmpxchgGetBytes ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetOffset"
  atomicCmpxchgGetOffset ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetPtr"
  atomicCmpxchgGetPtr ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetExpected"
  atomicCmpxchgGetExpected ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetReplacement"
  atomicCmpxchgGetReplacement ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicWaitGetPtr"
  atomicWaitGetPtr ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicWaitGetExpected"
  atomicWaitGetExpected ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicWaitGetTimeout"
  atomicWaitGetTimeout ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicWaitGetExpectedType"
  atomicWaitGetExpectedType ::
    Expression -> IO Type

foreign import ccall unsafe "BinaryenAtomicNotifyGetPtr"
  atomicNotifyGetPtr ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicNotifyGetNotifyCount"
  atomicNotifyGetNotifyCount ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenAtomicFenceGetOrder"
  atomicFenceGetOrder ::
    Expression -> IO Word8

foreign import ccall unsafe "BinaryenSIMDExtractGetOp"
  simdExtractGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenSIMDExtractGetVec"
  simdExtractGetVec ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDExtractGetIndex"
  simdExtractGetIndex ::
    Expression -> IO Word8

foreign import ccall unsafe "BinaryenSIMDReplaceGetOp"
  simdReplaceGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenSIMDReplaceGetVec"
  simdReplaceGetVec ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDReplaceGetIndex"
  simdReplaceGetIndex ::
    Expression -> IO Word8

foreign import ccall unsafe "BinaryenSIMDReplaceGetValue"
  simdReplaceGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDShuffleGetLeft"
  simdShuffleGetLeft ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDShuffleGetRight"
  simdShuffleGetRight ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDShuffleGetMask"
  simdShuffleGetMask ::
    Expression -> Ptr Word8 -> IO ()

foreign import ccall unsafe "BinaryenSIMDTernaryGetOp"
  simdTernaryGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenSIMDTernaryGetA"
  simdTernaryGetA ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDTernaryGetB"
  simdTernaryGetB ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDTernaryGetC"
  simdTernaryGetC ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDShiftGetOp"
  simdShiftGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenSIMDShiftGetVec"
  simdShiftGetVec ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDShiftGetShift"
  simdShiftGetShift ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenSIMDLoadGetOp"
  simdLoadGetOp ::
    Expression -> IO Op

foreign import ccall unsafe "BinaryenSIMDLoadGetOffset"
  simdLoadGetOffset ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenSIMDLoadGetAlign"
  simdLoadGetAlign ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenSIMDLoadGetPtr"
  simdLoadGetPtr ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryInitGetSegment"
  memoryInitGetSegment ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenMemoryInitGetDest"
  memoryInitGetDest ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryInitGetOffset"
  memoryInitGetOffset ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryInitGetSize"
  memoryInitGetSize ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenDataDropGetSegment"
  dataDropGetSegment ::
    Expression -> IO Word32

foreign import ccall unsafe "BinaryenMemoryCopyGetDest"
  memoryCopyGetDest ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryCopyGetSource"
  memoryCopyGetSource ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryCopyGetSize"
  memoryCopyGetSize ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryFillGetDest"
  memoryFillGetDest ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryFillGetValue"
  memoryFillGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenMemoryFillGetSize"
  memoryFillGetSize ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenRefIsNullGetValue"
  refIsNullGetValue ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenRefFuncGetFunc"
  refFuncGetFunc ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenTryGetBody"
  tryGetBody ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenTryGetCatchBody"
  tryGetCatchBody ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenThrowGetEvent"
  throwGetEvent ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenThrowGetOperand"
  throwGetOperand ::
    Expression -> Index -> IO Expression

foreign import ccall unsafe "BinaryenThrowGetNumOperands"
  throwGetNumOperands ::
    Expression -> IO Index

foreign import ccall unsafe "BinaryenRethrowGetExnref"
  rethrowGetExnref ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenBrOnExnGetEvent"
  brOnExnGetEvent ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenBrOnExnGetName"
  brOnExnGetName ::
    Expression -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenBrOnExnGetExnref"
  brOnExnGetExnref ::
    Expression -> IO Expression

foreign import ccall unsafe "BinaryenPushGetValue"
  pushGetValue ::
    Expression -> IO Expression
