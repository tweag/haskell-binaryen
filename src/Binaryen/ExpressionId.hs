{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Expression identifiers.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.
module Binaryen.ExpressionId where

import Data.Word (Word32)
import Foreign (Storable)

newtype ExpressionId = ExpressionId Word32
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenInvalidId" invalidId :: ExpressionId

foreign import ccall unsafe "BinaryenBlockId" blockId :: ExpressionId

foreign import ccall unsafe "BinaryenIfId" ifId :: ExpressionId

foreign import ccall unsafe "BinaryenLoopId" loopId :: ExpressionId

foreign import ccall unsafe "BinaryenBreakId" breakId :: ExpressionId

foreign import ccall unsafe "BinaryenSwitchId" switchId :: ExpressionId

foreign import ccall unsafe "BinaryenCallId" callId :: ExpressionId

foreign import ccall unsafe "BinaryenCallIndirectId" callIndirectId :: ExpressionId

foreign import ccall unsafe "BinaryenLocalGetId" localGetId :: ExpressionId

foreign import ccall unsafe "BinaryenLocalSetId" localSetId :: ExpressionId

foreign import ccall unsafe "BinaryenGlobalGetId" globalGetId :: ExpressionId

foreign import ccall unsafe "BinaryenGlobalSetId" globalSetId :: ExpressionId

foreign import ccall unsafe "BinaryenLoadId" loadId :: ExpressionId

foreign import ccall unsafe "BinaryenStoreId" storeId :: ExpressionId

foreign import ccall unsafe "BinaryenConstId" constId :: ExpressionId

foreign import ccall unsafe "BinaryenUnaryId" unaryId :: ExpressionId

foreign import ccall unsafe "BinaryenBinaryId" binaryId :: ExpressionId

foreign import ccall unsafe "BinaryenSelectId" selectId :: ExpressionId

foreign import ccall unsafe "BinaryenDropId" dropId :: ExpressionId

foreign import ccall unsafe "BinaryenReturnId" returnId :: ExpressionId

foreign import ccall unsafe "BinaryenMemorySizeId" memorySizeId :: ExpressionId

foreign import ccall unsafe "BinaryenMemoryGrowId" memoryGrowId :: ExpressionId

foreign import ccall unsafe "BinaryenNopId" nopId :: ExpressionId

foreign import ccall unsafe "BinaryenUnreachableId" unreachableId :: ExpressionId

foreign import ccall unsafe "BinaryenAtomicCmpxchgId" atomicCmpxchgId :: ExpressionId

foreign import ccall unsafe "BinaryenAtomicRMWId" atomicRMWId :: ExpressionId

foreign import ccall unsafe "BinaryenAtomicWaitId" atomicWaitId :: ExpressionId

foreign import ccall unsafe "BinaryenAtomicNotifyId" atomicNotifyId :: ExpressionId

foreign import ccall unsafe "BinaryenAtomicFenceId" atomicFenceId :: ExpressionId

foreign import ccall unsafe "BinaryenSIMDExtractId" simdExtractId :: ExpressionId

foreign import ccall unsafe "BinaryenSIMDReplaceId" simdReplaceId :: ExpressionId

foreign import ccall unsafe "BinaryenSIMDShuffleId" simdShuffleId :: ExpressionId

foreign import ccall unsafe "BinaryenSIMDTernaryId" simdTernaryId :: ExpressionId

foreign import ccall unsafe "BinaryenSIMDShiftId" simdShiftId :: ExpressionId

foreign import ccall unsafe "BinaryenSIMDLoadId" simdLoadId :: ExpressionId

foreign import ccall unsafe "BinaryenMemoryInitId" memoryInitId :: ExpressionId

foreign import ccall unsafe "BinaryenDataDropId" dataDropId :: ExpressionId

foreign import ccall unsafe "BinaryenMemoryCopyId" memoryCopyId :: ExpressionId

foreign import ccall unsafe "BinaryenMemoryFillId" memoryFillId :: ExpressionId

foreign import ccall unsafe "BinaryenRefNullId" refNullId :: ExpressionId

foreign import ccall unsafe "BinaryenRefIsNullId" refIsNullId :: ExpressionId

foreign import ccall unsafe "BinaryenRefFuncId" refFuncId :: ExpressionId

foreign import ccall unsafe "BinaryenTryId" tryId :: ExpressionId

foreign import ccall unsafe "BinaryenThrowId" throwId :: ExpressionId

foreign import ccall unsafe "BinaryenRethrowId" rethrowId :: ExpressionId

foreign import ccall unsafe "BinaryenBrOnExnId" brOnExnId :: ExpressionId

foreign import ccall unsafe "BinaryenPopId" popId :: ExpressionId
