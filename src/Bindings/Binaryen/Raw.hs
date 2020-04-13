{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bindings.Binaryen.Raw where

import Foreign
import Foreign.C

newtype Index = Index Word32
  deriving (Eq, Show)

newtype Type = Type Word32
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenTypeNone" typeNone :: Type
foreign import ccall unsafe "BinaryenTypeInt32" typeInt32 :: Type
foreign import ccall unsafe "BinaryenTypeInt64" typeInt64 :: Type
foreign import ccall unsafe "BinaryenTypeFloat32" typeFloat32 :: Type
foreign import ccall unsafe "BinaryenTypeFloat64" typeFloat64 :: Type
foreign import ccall unsafe "BinaryenTypeVec128" typeVec128 :: Type
foreign import ccall unsafe "BinaryenTypeFuncref" typeFuncref :: Type
foreign import ccall unsafe "BinaryenTypeAnyref" typeAnyref :: Type
foreign import ccall unsafe "BinaryenTypeNullref" typeNullref :: Type
foreign import ccall unsafe "BinaryenTypeExnref" typeExnref :: Type
foreign import ccall unsafe "BinaryenTypeUnreachable" typeUnreachable :: Type
foreign import ccall unsafe "BinaryenTypeAuto" typeAuto :: Type

foreign import ccall unsafe "BinaryenTypeCreate"
  typeCreate :: Ptr Type -> Word32 -> IO Type

foreign import ccall unsafe "BinaryenTypeArity"
  typeArity :: Type -> IO Word32

foreign import ccall unsafe "BinaryenTypeExpand"
  typeExpand :: Type -> Ptr Type -> IO ()

newtype ExpressionId = ExpressionId Word32
  deriving (Eq, Show)

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
foreign import ccall unsafe "BinaryenHostId" hostId :: ExpressionId
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
foreign import ccall unsafe "BinaryenPushId" pushId :: ExpressionId
foreign import ccall unsafe "BinaryenPopId" popId :: ExpressionId

newtype ExternalKind = ExternalKind Word32
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenExternalFunction" externalFunction :: ExternalKind
foreign import ccall unsafe "BinaryenExternalTable" externalTable :: ExternalKind
foreign import ccall unsafe "BinaryenExternalMemory" externalMemory :: ExternalKind
foreign import ccall unsafe "BinaryenExternalGlobal" externalGlobal :: ExternalKind
foreign import ccall unsafe "BinaryenExternalEvent" externalEvent :: ExternalKind

newtype Features = Features Word32
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenFeatureMVP" featureMVP :: Features
foreign import ccall unsafe "BinaryenFeatureAtomics" featureAtomics :: Features
foreign import ccall unsafe "BinaryenFeatureBulkMemory" featureBulkMemory :: Features
foreign import ccall unsafe "BinaryenFeatureMutableGlobals" featureMutableGlobals :: Features
foreign import ccall unsafe "BinaryenFeatureNontrappingFPToInt" featureNontrappingFPToInt :: Features
foreign import ccall unsafe "BinaryenFeatureSignExt" featureSignExt :: Features
foreign import ccall unsafe "BinaryenFeatureSIMD128" featureSIMD128 :: Features
foreign import ccall unsafe "BinaryenFeatureExceptionHandling" featureExceptionHandling :: Features
foreign import ccall unsafe "BinaryenFeatureTailCall" featureTailCall :: Features
foreign import ccall unsafe "BinaryenFeatureReferenceTypes" featureReferenceTypes :: Features
foreign import ccall unsafe "BinaryenFeatureAll" featureAll :: Features

newtype Module = Module (Ptr Module)
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenModuleCreate"
  moduleCreate ::
    IO Module

foreign import ccall unsafe "BinaryenModuleDispose"
  moduleDispose ::
    Module -> IO ()

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

newtype Op = Op Int32
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenClzInt32" clzInt32 :: Op
foreign import ccall unsafe "BinaryenCtzInt32" ctzInt32 :: Op
foreign import ccall unsafe "BinaryenPopcntInt32" popcntInt32 :: Op
foreign import ccall unsafe "BinaryenNegFloat32" negFloat32 :: Op
foreign import ccall unsafe "BinaryenAbsFloat32" absFloat32 :: Op
foreign import ccall unsafe "BinaryenCeilFloat32" ceilFloat32 :: Op
foreign import ccall unsafe "BinaryenFloorFloat32" floorFloat32 :: Op
foreign import ccall unsafe "BinaryenTruncFloat32" truncFloat32 :: Op
foreign import ccall unsafe "BinaryenNearestFloat32" nearestFloat32 :: Op
foreign import ccall unsafe "BinaryenSqrtFloat32" sqrtFloat32 :: Op
foreign import ccall unsafe "BinaryenEqZInt32" eqZInt32 :: Op
foreign import ccall unsafe "BinaryenClzInt64" clzInt64 :: Op
foreign import ccall unsafe "BinaryenCtzInt64" ctzInt64 :: Op
foreign import ccall unsafe "BinaryenPopcntInt64" popcntInt64 :: Op
foreign import ccall unsafe "BinaryenNegFloat64" negFloat64 :: Op
foreign import ccall unsafe "BinaryenAbsFloat64" absFloat64 :: Op
foreign import ccall unsafe "BinaryenCeilFloat64" ceilFloat64 :: Op
foreign import ccall unsafe "BinaryenFloorFloat64" floorFloat64 :: Op
foreign import ccall unsafe "BinaryenTruncFloat64" truncFloat64 :: Op
foreign import ccall unsafe "BinaryenNearestFloat64" nearestFloat64 :: Op
foreign import ccall unsafe "BinaryenSqrtFloat64" sqrtFloat64 :: Op
foreign import ccall unsafe "BinaryenEqZInt64" eqZInt64 :: Op
foreign import ccall unsafe "BinaryenExtendSInt32" extendSInt32 :: Op
foreign import ccall unsafe "BinaryenExtendUInt32" extendUInt32 :: Op
foreign import ccall unsafe "BinaryenWrapInt64" wrapInt64 :: Op
foreign import ccall unsafe "BinaryenTruncSFloat32ToInt32" truncSFloat32ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncSFloat32ToInt64" truncSFloat32ToInt64 :: Op
foreign import ccall unsafe "BinaryenTruncUFloat32ToInt32" truncUFloat32ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncUFloat32ToInt64" truncUFloat32ToInt64 :: Op
foreign import ccall unsafe "BinaryenTruncSFloat64ToInt32" truncSFloat64ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncSFloat64ToInt64" truncSFloat64ToInt64 :: Op
foreign import ccall unsafe "BinaryenTruncUFloat64ToInt32" truncUFloat64ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncUFloat64ToInt64" truncUFloat64ToInt64 :: Op
foreign import ccall unsafe "BinaryenReinterpretFloat32" reinterpretFloat32 :: Op
foreign import ccall unsafe "BinaryenReinterpretFloat64" reinterpretFloat64 :: Op
foreign import ccall unsafe "BinaryenConvertSInt32ToFloat32" convertSInt32ToFloat32 :: Op
foreign import ccall unsafe "BinaryenConvertSInt32ToFloat64" convertSInt32ToFloat64 :: Op
foreign import ccall unsafe "BinaryenConvertUInt32ToFloat32" convertUInt32ToFloat32 :: Op
foreign import ccall unsafe "BinaryenConvertUInt32ToFloat64" convertUInt32ToFloat64 :: Op
foreign import ccall unsafe "BinaryenConvertSInt64ToFloat32" convertSInt64ToFloat32 :: Op
foreign import ccall unsafe "BinaryenConvertSInt64ToFloat64" convertSInt64ToFloat64 :: Op
foreign import ccall unsafe "BinaryenConvertUInt64ToFloat32" convertUInt64ToFloat32 :: Op
foreign import ccall unsafe "BinaryenConvertUInt64ToFloat64" convertUInt64ToFloat64 :: Op
foreign import ccall unsafe "BinaryenPromoteFloat32" promoteFloat32 :: Op
foreign import ccall unsafe "BinaryenDemoteFloat64" demoteFloat64 :: Op
foreign import ccall unsafe "BinaryenReinterpretInt32" reinterpretInt32 :: Op
foreign import ccall unsafe "BinaryenReinterpretInt64" reinterpretInt64 :: Op
foreign import ccall unsafe "BinaryenExtendS8Int32" extendS8Int32 :: Op
foreign import ccall unsafe "BinaryenExtendS16Int32" extendS16Int32 :: Op
foreign import ccall unsafe "BinaryenExtendS8Int64" extendS8Int64 :: Op
foreign import ccall unsafe "BinaryenExtendS16Int64" extendS16Int64 :: Op
foreign import ccall unsafe "BinaryenExtendS32Int64" extendS32Int64 :: Op
foreign import ccall unsafe "BinaryenAddInt32" addInt32 :: Op
foreign import ccall unsafe "BinaryenSubInt32" subInt32 :: Op
foreign import ccall unsafe "BinaryenMulInt32" mulInt32 :: Op
foreign import ccall unsafe "BinaryenDivSInt32" divSInt32 :: Op
foreign import ccall unsafe "BinaryenDivUInt32" divUInt32 :: Op
foreign import ccall unsafe "BinaryenRemSInt32" remSInt32 :: Op
foreign import ccall unsafe "BinaryenRemUInt32" remUInt32 :: Op
foreign import ccall unsafe "BinaryenAndInt32" andInt32 :: Op
foreign import ccall unsafe "BinaryenOrInt32" orInt32 :: Op
foreign import ccall unsafe "BinaryenXorInt32" xorInt32 :: Op
foreign import ccall unsafe "BinaryenShlInt32" shlInt32 :: Op
foreign import ccall unsafe "BinaryenShrUInt32" shrUInt32 :: Op
foreign import ccall unsafe "BinaryenShrSInt32" shrSInt32 :: Op
foreign import ccall unsafe "BinaryenRotLInt32" rotLInt32 :: Op
foreign import ccall unsafe "BinaryenRotRInt32" rotRInt32 :: Op
foreign import ccall unsafe "BinaryenEqInt32" eqInt32 :: Op
foreign import ccall unsafe "BinaryenNeInt32" neInt32 :: Op
foreign import ccall unsafe "BinaryenLtSInt32" ltSInt32 :: Op
foreign import ccall unsafe "BinaryenLtUInt32" ltUInt32 :: Op
foreign import ccall unsafe "BinaryenLeSInt32" leSInt32 :: Op
foreign import ccall unsafe "BinaryenLeUInt32" leUInt32 :: Op
foreign import ccall unsafe "BinaryenGtSInt32" gtSInt32 :: Op
foreign import ccall unsafe "BinaryenGtUInt32" gtUInt32 :: Op
foreign import ccall unsafe "BinaryenGeSInt32" geSInt32 :: Op
foreign import ccall unsafe "BinaryenGeUInt32" geUInt32 :: Op
foreign import ccall unsafe "BinaryenAddInt64" addInt64 :: Op
foreign import ccall unsafe "BinaryenSubInt64" subInt64 :: Op
foreign import ccall unsafe "BinaryenMulInt64" mulInt64 :: Op
foreign import ccall unsafe "BinaryenDivSInt64" divSInt64 :: Op
foreign import ccall unsafe "BinaryenDivUInt64" divUInt64 :: Op
foreign import ccall unsafe "BinaryenRemSInt64" remSInt64 :: Op
foreign import ccall unsafe "BinaryenRemUInt64" remUInt64 :: Op
foreign import ccall unsafe "BinaryenAndInt64" andInt64 :: Op
foreign import ccall unsafe "BinaryenOrInt64" orInt64 :: Op
foreign import ccall unsafe "BinaryenXorInt64" xorInt64 :: Op
foreign import ccall unsafe "BinaryenShlInt64" shlInt64 :: Op
foreign import ccall unsafe "BinaryenShrUInt64" shrUInt64 :: Op
foreign import ccall unsafe "BinaryenShrSInt64" shrSInt64 :: Op
foreign import ccall unsafe "BinaryenRotLInt64" rotLInt64 :: Op
foreign import ccall unsafe "BinaryenRotRInt64" rotRInt64 :: Op
foreign import ccall unsafe "BinaryenEqInt64" eqInt64 :: Op
foreign import ccall unsafe "BinaryenNeInt64" neInt64 :: Op
foreign import ccall unsafe "BinaryenLtSInt64" ltSInt64 :: Op
foreign import ccall unsafe "BinaryenLtUInt64" ltUInt64 :: Op
foreign import ccall unsafe "BinaryenLeSInt64" leSInt64 :: Op
foreign import ccall unsafe "BinaryenLeUInt64" leUInt64 :: Op
foreign import ccall unsafe "BinaryenGtSInt64" gtSInt64 :: Op
foreign import ccall unsafe "BinaryenGtUInt64" gtUInt64 :: Op
foreign import ccall unsafe "BinaryenGeSInt64" geSInt64 :: Op
foreign import ccall unsafe "BinaryenGeUInt64" geUInt64 :: Op
foreign import ccall unsafe "BinaryenAddFloat32" addFloat32 :: Op
foreign import ccall unsafe "BinaryenSubFloat32" subFloat32 :: Op
foreign import ccall unsafe "BinaryenMulFloat32" mulFloat32 :: Op
foreign import ccall unsafe "BinaryenDivFloat32" divFloat32 :: Op
foreign import ccall unsafe "BinaryenCopySignFloat32" copySignFloat32 :: Op
foreign import ccall unsafe "BinaryenMinFloat32" minFloat32 :: Op
foreign import ccall unsafe "BinaryenMaxFloat32" maxFloat32 :: Op
foreign import ccall unsafe "BinaryenEqFloat32" eqFloat32 :: Op
foreign import ccall unsafe "BinaryenNeFloat32" neFloat32 :: Op
foreign import ccall unsafe "BinaryenLtFloat32" ltFloat32 :: Op
foreign import ccall unsafe "BinaryenLeFloat32" leFloat32 :: Op
foreign import ccall unsafe "BinaryenGtFloat32" gtFloat32 :: Op
foreign import ccall unsafe "BinaryenGeFloat32" geFloat32 :: Op
foreign import ccall unsafe "BinaryenAddFloat64" addFloat64 :: Op
foreign import ccall unsafe "BinaryenSubFloat64" subFloat64 :: Op
foreign import ccall unsafe "BinaryenMulFloat64" mulFloat64 :: Op
foreign import ccall unsafe "BinaryenDivFloat64" divFloat64 :: Op
foreign import ccall unsafe "BinaryenCopySignFloat64" copySignFloat64 :: Op
foreign import ccall unsafe "BinaryenMinFloat64" minFloat64 :: Op
foreign import ccall unsafe "BinaryenMaxFloat64" maxFloat64 :: Op
foreign import ccall unsafe "BinaryenEqFloat64" eqFloat64 :: Op
foreign import ccall unsafe "BinaryenNeFloat64" neFloat64 :: Op
foreign import ccall unsafe "BinaryenLtFloat64" ltFloat64 :: Op
foreign import ccall unsafe "BinaryenLeFloat64" leFloat64 :: Op
foreign import ccall unsafe "BinaryenGtFloat64" gtFloat64 :: Op
foreign import ccall unsafe "BinaryenGeFloat64" geFloat64 :: Op
foreign import ccall unsafe "BinaryenMemorySize" memorySize :: Op
foreign import ccall unsafe "BinaryenMemoryGrow" memoryGrow :: Op
foreign import ccall unsafe "BinaryenAtomicRMWAdd" atomicRMWAdd :: Op
foreign import ccall unsafe "BinaryenAtomicRMWSub" atomicRMWSub :: Op
foreign import ccall unsafe "BinaryenAtomicRMWAnd" atomicRMWAnd :: Op
foreign import ccall unsafe "BinaryenAtomicRMWOr" atomicRMWOr :: Op
foreign import ccall unsafe "BinaryenAtomicRMWXor" atomicRMWXor :: Op
foreign import ccall unsafe "BinaryenAtomicRMWXchg" atomicRMWXchg :: Op
foreign import ccall unsafe "BinaryenTruncSatSFloat32ToInt32" truncSatSFloat32ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncSatSFloat32ToInt64" truncSatSFloat32ToInt64 :: Op
foreign import ccall unsafe "BinaryenTruncSatUFloat32ToInt32" truncSatUFloat32ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncSatUFloat32ToInt64" truncSatUFloat32ToInt64 :: Op
foreign import ccall unsafe "BinaryenTruncSatSFloat64ToInt32" truncSatSFloat64ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncSatSFloat64ToInt64" truncSatSFloat64ToInt64 :: Op
foreign import ccall unsafe "BinaryenTruncSatUFloat64ToInt32" truncSatUFloat64ToInt32 :: Op
foreign import ccall unsafe "BinaryenTruncSatUFloat64ToInt64" truncSatUFloat64ToInt64 :: Op
foreign import ccall unsafe "BinaryenSplatVecI8x16" splatVecI8x16 :: Op
foreign import ccall unsafe "BinaryenExtractLaneSVecI8x16" extractLaneSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenExtractLaneUVecI8x16" extractLaneUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenReplaceLaneVecI8x16" replaceLaneVecI8x16 :: Op
foreign import ccall unsafe "BinaryenSplatVecI16x8" splatVecI16x8 :: Op
foreign import ccall unsafe "BinaryenExtractLaneSVecI16x8" extractLaneSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenExtractLaneUVecI16x8" extractLaneUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenReplaceLaneVecI16x8" replaceLaneVecI16x8 :: Op
foreign import ccall unsafe "BinaryenSplatVecI32x4" splatVecI32x4 :: Op
foreign import ccall unsafe "BinaryenExtractLaneVecI32x4" extractLaneVecI32x4 :: Op
foreign import ccall unsafe "BinaryenReplaceLaneVecI32x4" replaceLaneVecI32x4 :: Op
foreign import ccall unsafe "BinaryenSplatVecI64x2" splatVecI64x2 :: Op
foreign import ccall unsafe "BinaryenExtractLaneVecI64x2" extractLaneVecI64x2 :: Op
foreign import ccall unsafe "BinaryenReplaceLaneVecI64x2" replaceLaneVecI64x2 :: Op
foreign import ccall unsafe "BinaryenSplatVecF32x4" splatVecF32x4 :: Op
foreign import ccall unsafe "BinaryenExtractLaneVecF32x4" extractLaneVecF32x4 :: Op
foreign import ccall unsafe "BinaryenReplaceLaneVecF32x4" replaceLaneVecF32x4 :: Op
foreign import ccall unsafe "BinaryenSplatVecF64x2" splatVecF64x2 :: Op
foreign import ccall unsafe "BinaryenExtractLaneVecF64x2" extractLaneVecF64x2 :: Op
foreign import ccall unsafe "BinaryenReplaceLaneVecF64x2" replaceLaneVecF64x2 :: Op
foreign import ccall unsafe "BinaryenEqVecI8x16" eqVecI8x16 :: Op
foreign import ccall unsafe "BinaryenNeVecI8x16" neVecI8x16 :: Op
foreign import ccall unsafe "BinaryenLtSVecI8x16" ltSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenLtUVecI8x16" ltUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenGtSVecI8x16" gtSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenGtUVecI8x16" gtUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenLeSVecI8x16" leSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenLeUVecI8x16" leUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenGeSVecI8x16" geSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenGeUVecI8x16" geUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenEqVecI16x8" eqVecI16x8 :: Op
foreign import ccall unsafe "BinaryenNeVecI16x8" neVecI16x8 :: Op
foreign import ccall unsafe "BinaryenLtSVecI16x8" ltSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenLtUVecI16x8" ltUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenGtSVecI16x8" gtSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenGtUVecI16x8" gtUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenLeSVecI16x8" leSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenLeUVecI16x8" leUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenGeSVecI16x8" geSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenGeUVecI16x8" geUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenEqVecI32x4" eqVecI32x4 :: Op
foreign import ccall unsafe "BinaryenNeVecI32x4" neVecI32x4 :: Op
foreign import ccall unsafe "BinaryenLtSVecI32x4" ltSVecI32x4 :: Op
foreign import ccall unsafe "BinaryenLtUVecI32x4" ltUVecI32x4 :: Op
foreign import ccall unsafe "BinaryenGtSVecI32x4" gtSVecI32x4 :: Op
foreign import ccall unsafe "BinaryenGtUVecI32x4" gtUVecI32x4 :: Op
foreign import ccall unsafe "BinaryenLeSVecI32x4" leSVecI32x4 :: Op
foreign import ccall unsafe "BinaryenLeUVecI32x4" leUVecI32x4 :: Op
foreign import ccall unsafe "BinaryenGeSVecI32x4" geSVecI32x4 :: Op
foreign import ccall unsafe "BinaryenGeUVecI32x4" geUVecI32x4 :: Op
foreign import ccall unsafe "BinaryenEqVecF32x4" eqVecF32x4 :: Op
foreign import ccall unsafe "BinaryenNeVecF32x4" neVecF32x4 :: Op
foreign import ccall unsafe "BinaryenLtVecF32x4" ltVecF32x4 :: Op
foreign import ccall unsafe "BinaryenGtVecF32x4" gtVecF32x4 :: Op
foreign import ccall unsafe "BinaryenLeVecF32x4" leVecF32x4 :: Op
foreign import ccall unsafe "BinaryenGeVecF32x4" geVecF32x4 :: Op
foreign import ccall unsafe "BinaryenEqVecF64x2" eqVecF64x2 :: Op
foreign import ccall unsafe "BinaryenNeVecF64x2" neVecF64x2 :: Op
foreign import ccall unsafe "BinaryenLtVecF64x2" ltVecF64x2 :: Op
foreign import ccall unsafe "BinaryenGtVecF64x2" gtVecF64x2 :: Op
foreign import ccall unsafe "BinaryenLeVecF64x2" leVecF64x2 :: Op
foreign import ccall unsafe "BinaryenGeVecF64x2" geVecF64x2 :: Op
foreign import ccall unsafe "BinaryenNotVec128" notVec128 :: Op
foreign import ccall unsafe "BinaryenAndVec128" andVec128 :: Op
foreign import ccall unsafe "BinaryenOrVec128" orVec128 :: Op
foreign import ccall unsafe "BinaryenXorVec128" xorVec128 :: Op
foreign import ccall unsafe "BinaryenAndNotVec128" andNotVec128 :: Op
foreign import ccall unsafe "BinaryenBitselectVec128" bitselectVec128 :: Op
foreign import ccall unsafe "BinaryenNegVecI8x16" negVecI8x16 :: Op
foreign import ccall unsafe "BinaryenAnyTrueVecI8x16" anyTrueVecI8x16 :: Op
foreign import ccall unsafe "BinaryenAllTrueVecI8x16" allTrueVecI8x16 :: Op
foreign import ccall unsafe "BinaryenShlVecI8x16" shlVecI8x16 :: Op
foreign import ccall unsafe "BinaryenShrSVecI8x16" shrSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenShrUVecI8x16" shrUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenAddVecI8x16" addVecI8x16 :: Op
foreign import ccall unsafe "BinaryenAddSatSVecI8x16" addSatSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenAddSatUVecI8x16" addSatUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenSubVecI8x16" subVecI8x16 :: Op
foreign import ccall unsafe "BinaryenSubSatSVecI8x16" subSatSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenSubSatUVecI8x16" subSatUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenMulVecI8x16" mulVecI8x16 :: Op
foreign import ccall unsafe "BinaryenMinSVecI8x16" minSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenMinUVecI8x16" minUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenMaxSVecI8x16" maxSVecI8x16 :: Op
foreign import ccall unsafe "BinaryenMaxUVecI8x16" maxUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenAvgrUVecI8x16" avgrUVecI8x16 :: Op
foreign import ccall unsafe "BinaryenNegVecI16x8" negVecI16x8 :: Op
foreign import ccall unsafe "BinaryenAnyTrueVecI16x8" anyTrueVecI16x8 :: Op
foreign import ccall unsafe "BinaryenAllTrueVecI16x8" allTrueVecI16x8 :: Op
foreign import ccall unsafe "BinaryenShlVecI16x8" shlVecI16x8 :: Op
foreign import ccall unsafe "BinaryenShrSVecI16x8" shrSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenShrUVecI16x8" shrUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenAddVecI16x8" addVecI16x8 :: Op
foreign import ccall unsafe "BinaryenAddSatSVecI16x8" addSatSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenAddSatUVecI16x8" addSatUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenSubVecI16x8" subVecI16x8 :: Op
foreign import ccall unsafe "BinaryenSubSatSVecI16x8" subSatSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenSubSatUVecI16x8" subSatUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenMulVecI16x8" mulVecI16x8 :: Op
foreign import ccall unsafe "BinaryenMinSVecI16x8" minSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenMinUVecI16x8" minUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenMaxSVecI16x8" maxSVecI16x8 :: Op
foreign import ccall unsafe "BinaryenMaxUVecI16x8" maxUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenAvgrUVecI16x8" avgrUVecI16x8 :: Op
foreign import ccall unsafe "BinaryenNegVecI32x4" negVecI32x4 :: Op
foreign import ccall unsafe "BinaryenAnyTrueVecI32x4" anyTrueVecI32x4 :: Op
foreign import ccall unsafe "BinaryenAllTrueVecI32x4" allTrueVecI32x4 :: Op
foreign import ccall unsafe "BinaryenShlVecI32x4" shlVecI32x4 :: Op
foreign import ccall unsafe "BinaryenShrSVecI32x4" shrSVecI32x4 :: Op
foreign import ccall unsafe "BinaryenShrUVecI32x4" shrUVecI32x4 :: Op
foreign import ccall unsafe "BinaryenAddVecI32x4" addVecI32x4 :: Op
foreign import ccall unsafe "BinaryenSubVecI32x4" subVecI32x4 :: Op
foreign import ccall unsafe "BinaryenMulVecI32x4" mulVecI32x4 :: Op
foreign import ccall unsafe "BinaryenMinSVecI32x4" minSVecI32x4 :: Op
foreign import ccall unsafe "BinaryenMinUVecI32x4" minUVecI32x4 :: Op
foreign import ccall unsafe "BinaryenMaxSVecI32x4" maxSVecI32x4 :: Op
foreign import ccall unsafe "BinaryenMaxUVecI32x4" maxUVecI32x4 :: Op
foreign import ccall unsafe "BinaryenDotSVecI16x8ToVecI32x4" dotSVecI16x8ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenNegVecI64x2" negVecI64x2 :: Op
foreign import ccall unsafe "BinaryenAnyTrueVecI64x2" anyTrueVecI64x2 :: Op
foreign import ccall unsafe "BinaryenAllTrueVecI64x2" allTrueVecI64x2 :: Op
foreign import ccall unsafe "BinaryenShlVecI64x2" shlVecI64x2 :: Op
foreign import ccall unsafe "BinaryenShrSVecI64x2" shrSVecI64x2 :: Op
foreign import ccall unsafe "BinaryenShrUVecI64x2" shrUVecI64x2 :: Op
foreign import ccall unsafe "BinaryenAddVecI64x2" addVecI64x2 :: Op
foreign import ccall unsafe "BinaryenSubVecI64x2" subVecI64x2 :: Op
foreign import ccall unsafe "BinaryenAbsVecF32x4" absVecF32x4 :: Op
foreign import ccall unsafe "BinaryenNegVecF32x4" negVecF32x4 :: Op
foreign import ccall unsafe "BinaryenSqrtVecF32x4" sqrtVecF32x4 :: Op
foreign import ccall unsafe "BinaryenQFMAVecF32x4" qFMAVecF32x4 :: Op
foreign import ccall unsafe "BinaryenQFMSVecF32x4" qFMSVecF32x4 :: Op
foreign import ccall unsafe "BinaryenAddVecF32x4" addVecF32x4 :: Op
foreign import ccall unsafe "BinaryenSubVecF32x4" subVecF32x4 :: Op
foreign import ccall unsafe "BinaryenMulVecF32x4" mulVecF32x4 :: Op
foreign import ccall unsafe "BinaryenDivVecF32x4" divVecF32x4 :: Op
foreign import ccall unsafe "BinaryenMinVecF32x4" minVecF32x4 :: Op
foreign import ccall unsafe "BinaryenMaxVecF32x4" maxVecF32x4 :: Op
foreign import ccall unsafe "BinaryenAbsVecF64x2" absVecF64x2 :: Op
foreign import ccall unsafe "BinaryenNegVecF64x2" negVecF64x2 :: Op
foreign import ccall unsafe "BinaryenSqrtVecF64x2" sqrtVecF64x2 :: Op
foreign import ccall unsafe "BinaryenQFMAVecF64x2" qFMAVecF64x2 :: Op
foreign import ccall unsafe "BinaryenQFMSVecF64x2" qFMSVecF64x2 :: Op
foreign import ccall unsafe "BinaryenAddVecF64x2" addVecF64x2 :: Op
foreign import ccall unsafe "BinaryenSubVecF64x2" subVecF64x2 :: Op
foreign import ccall unsafe "BinaryenMulVecF64x2" mulVecF64x2 :: Op
foreign import ccall unsafe "BinaryenDivVecF64x2" divVecF64x2 :: Op
foreign import ccall unsafe "BinaryenMinVecF64x2" minVecF64x2 :: Op
foreign import ccall unsafe "BinaryenMaxVecF64x2" maxVecF64x2 :: Op
foreign import ccall unsafe "BinaryenTruncSatSVecF32x4ToVecI32x4" truncSatSVecF32x4ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenTruncSatUVecF32x4ToVecI32x4" truncSatUVecF32x4ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenTruncSatSVecF64x2ToVecI64x2" truncSatSVecF64x2ToVecI64x2 :: Op
foreign import ccall unsafe "BinaryenTruncSatUVecF64x2ToVecI64x2" truncSatUVecF64x2ToVecI64x2 :: Op
foreign import ccall unsafe "BinaryenConvertSVecI32x4ToVecF32x4" convertSVecI32x4ToVecF32x4 :: Op
foreign import ccall unsafe "BinaryenConvertUVecI32x4ToVecF32x4" convertUVecI32x4ToVecF32x4 :: Op
foreign import ccall unsafe "BinaryenConvertSVecI64x2ToVecF64x2" convertSVecI64x2ToVecF64x2 :: Op
foreign import ccall unsafe "BinaryenConvertUVecI64x2ToVecF64x2" convertUVecI64x2ToVecF64x2 :: Op
foreign import ccall unsafe "BinaryenLoadSplatVec8x16" loadSplatVec8x16 :: Op
foreign import ccall unsafe "BinaryenLoadSplatVec16x8" loadSplatVec16x8 :: Op
foreign import ccall unsafe "BinaryenLoadSplatVec32x4" loadSplatVec32x4 :: Op
foreign import ccall unsafe "BinaryenLoadSplatVec64x2" loadSplatVec64x2 :: Op
foreign import ccall unsafe "BinaryenLoadExtSVec8x8ToVecI16x8" loadExtSVec8x8ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenLoadExtUVec8x8ToVecI16x8" loadExtUVec8x8ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenLoadExtSVec16x4ToVecI32x4" loadExtSVec16x4ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenLoadExtUVec16x4ToVecI32x4" loadExtUVec16x4ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenLoadExtSVec32x2ToVecI64x2" loadExtSVec32x2ToVecI64x2 :: Op
foreign import ccall unsafe "BinaryenLoadExtUVec32x2ToVecI64x2" loadExtUVec32x2ToVecI64x2 :: Op
foreign import ccall unsafe "BinaryenNarrowSVecI16x8ToVecI8x16" narrowSVecI16x8ToVecI8x16 :: Op
foreign import ccall unsafe "BinaryenNarrowUVecI16x8ToVecI8x16" narrowUVecI16x8ToVecI8x16 :: Op
foreign import ccall unsafe "BinaryenNarrowSVecI32x4ToVecI16x8" narrowSVecI32x4ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenNarrowUVecI32x4ToVecI16x8" narrowUVecI32x4ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenWidenLowSVecI8x16ToVecI16x8" widenLowSVecI8x16ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenWidenHighSVecI8x16ToVecI16x8" widenHighSVecI8x16ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenWidenLowUVecI8x16ToVecI16x8" widenLowUVecI8x16ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenWidenHighUVecI8x16ToVecI16x8" widenHighUVecI8x16ToVecI16x8 :: Op
foreign import ccall unsafe "BinaryenWidenLowSVecI16x8ToVecI32x4" widenLowSVecI16x8ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenWidenHighSVecI16x8ToVecI32x4" widenHighSVecI16x8ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenWidenLowUVecI16x8ToVecI32x4" widenLowUVecI16x8ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenWidenHighUVecI16x8ToVecI32x4" widenHighUVecI16x8ToVecI32x4 :: Op
foreign import ccall unsafe "BinaryenSwizzleVec8x16" swizzleVec8x16 :: Op

newtype Expression = Expression (Ptr Expression)
  deriving (Eq, Show)

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
  expressionGetId ::
    Expression -> IO ExpressionId

foreign import ccall unsafe "BinaryenExpressionGetType"
  expressionGetType ::
    Expression -> IO Type

foreign import ccall unsafe "BinaryenExpressionPrint"
  expressionPrint ::
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

newtype Function = Function (Ptr Function)
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenAddFunction"
  addFunction ::
    Module ->
    Ptr CChar ->
    Type ->
    Type ->
    Ptr Type ->
    Index ->
    Expression ->
    IO Function

foreign import ccall unsafe "BinaryenGetFunction"
  getFunction ::
    Module -> Ptr CChar -> IO Function

foreign import ccall unsafe "BinaryenRemoveFunction"
  removeFunction ::
    Module -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenGetNumFunctions"
  getNumFunctions ::
    Module -> IO Word32

foreign import ccall unsafe "BinaryenGetFunctionByIndex"
  getFunctionByIndex ::
    Module -> Index -> IO Function

foreign import ccall unsafe "BinaryenAddFunctionImport"
  addFunctionImport ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Ptr CChar ->
    Type ->
    Type ->
    IO ()

foreign import ccall unsafe "BinaryenAddTableImport"
  addTableImport ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Ptr CChar ->
    IO ()

foreign import ccall unsafe "BinaryenAddMemoryImport"
  addMemoryImport ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Ptr CChar ->
    Word8 ->
    IO ()

foreign import ccall unsafe "BinaryenAddGlobalImport"
  addGlobalImport ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Ptr CChar ->
    Type ->
    CInt ->
    IO ()

foreign import ccall unsafe "BinaryenAddEventImport"
  addEventImport ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Ptr CChar ->
    Word32 ->
    Type ->
    Type ->
    IO ()

newtype Export = Export (Ptr Export)
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenAddFunctionExport"
  addFunctionExport ::
    Module -> Ptr CChar -> Ptr CChar -> IO Export

foreign import ccall unsafe "BinaryenAddTableExport"
  addTableExport ::
    Module -> Ptr CChar -> Ptr CChar -> IO Export

foreign import ccall unsafe "BinaryenAddMemoryExport"
  addMemoryExport ::
    Module -> Ptr CChar -> Ptr CChar -> IO Export

foreign import ccall unsafe "BinaryenAddGlobalExport"
  addGlobalExport ::
    Module -> Ptr CChar -> Ptr CChar -> IO Export

foreign import ccall unsafe "BinaryenAddEventExport"
  addEventExport ::
    Module -> Ptr CChar -> Ptr CChar -> IO Export

foreign import ccall unsafe "BinaryenRemoveExport"
  removeExport ::
    Module -> Ptr CChar -> IO ()

newtype Global = Global (Ptr Global)
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenAddGlobal"
  addGlobal ::
    Module ->
    Ptr CChar ->
    Type ->
    Int8 ->
    Expression ->
    IO Global

foreign import ccall unsafe "BinaryenGetGlobal"
  getGlobal ::
    Module -> Ptr CChar -> IO Global

foreign import ccall unsafe "BinaryenRemoveGlobal"
  removeGlobal ::
    Module -> Ptr CChar -> IO ()

newtype Event = Event (Ptr Event)
  deriving (Eq, Show)

foreign import ccall unsafe "BinaryenAddEvent"
  addEvent ::
    Module ->
    Ptr CChar ->
    Word32 ->
    Type ->
    Type ->
    IO Event

foreign import ccall unsafe "BinaryenGetEvent"
  getEvent ::
    Module -> Ptr CChar -> IO Event

foreign import ccall unsafe "BinaryenRemoveEvent"
  removeEvent ::
    Module -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenSetFunctionTable"
  setFunctionTable ::
    Module ->
    Index ->
    Index ->
    Ptr (Ptr CChar) ->
    Index ->
    Expression ->
    IO ()

foreign import ccall unsafe "BinaryenIsFunctionTableImported"
  isFunctionTableImported ::
    Module -> IO CInt

foreign import ccall unsafe "BinaryenGetNumFunctionTableSegments"
  getNumFunctionTableSegments ::
    Module -> IO Index

foreign import ccall unsafe "BinaryenGetFunctionTableSegmentOffset"
  getFunctionTableSegmentOffset ::
    Module -> Index -> IO Expression

foreign import ccall unsafe "BinaryenGetFunctionTableSegmentLength"
  getFunctionTableSegmentLength ::
    Module -> Index -> IO Index

foreign import ccall unsafe "BinaryenGetFunctionTableSegmentData"
  getFunctionTableSegmentData ::
    Module ->
    Index ->
    Index ->
    IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSetMemory"
  setMemory ::
    Module ->
    Index ->
    Index ->
    Ptr CChar ->
    Ptr (Ptr CChar) ->
    Ptr Int8 ->
    Ptr Expression ->
    Ptr Index ->
    Index ->
    Word8 ->
    IO ()

foreign import ccall unsafe "BinaryenGetNumMemorySegments"
  getNumMemorySegments ::
    Module -> IO Word32

foreign import ccall unsafe "BinaryenGetMemorySegmentByteOffset"
  getMemorySegmentByteOffset ::
    Module -> Index -> IO Word32

foreign import ccall unsafe "BinaryenGetMemorySegmentByteLength"
  getMemorySegmentByteLength ::
    Module -> Index -> IO CSize

foreign import ccall unsafe "BinaryenGetMemorySegmentPassive"
  getMemorySegmentPassive ::
    Module -> Index -> IO CInt

foreign import ccall unsafe "BinaryenCopyMemorySegmentData"
  copyMemorySegmentData ::
    Module -> Index -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenSetStart"
  setStart ::
    Module -> Function -> IO ()

foreign import ccall unsafe "BinaryenModuleGetFeatures"
  moduleGetFeatures ::
    Module -> IO Features

foreign import ccall unsafe "BinaryenModuleSetFeatures"
  moduleSetFeatures ::
    Module -> Features -> IO ()

foreign import ccall unsafe "BinaryenModuleParse"
  moduleParse ::
    Ptr CChar -> IO Module

foreign import ccall unsafe "BinaryenModulePrint"
  modulePrint ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModulePrintAsmjs"
  modulePrintAsmjs ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModuleValidate"
  moduleValidate ::
    Module -> IO CInt

foreign import ccall unsafe "BinaryenModuleOptimize"
  moduleOptimize ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenGetOptimizeLevel"
  getOptimizeLevel ::
    IO CInt

foreign import ccall unsafe "BinaryenSetOptimizeLevel"
  setOptimizeLevel ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenGetShrinkLevel"
  getShrinkLevel ::
    IO CInt

foreign import ccall unsafe "BinaryenSetShrinkLevel"
  setShrinkLevel ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenGetDebugInfo"
  getDebugInfo ::
    IO CInt

foreign import ccall unsafe "BinaryenSetDebugInfo"
  setDebugInfo ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenGetLowMemoryUnused"
  getLowMemoryUnused ::
    IO CInt

foreign import ccall unsafe "BinaryenSetLowMemoryUnused"
  setLowMemoryUnused ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenGetPassArgument"
  getPassArgument ::
    Ptr CChar -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSetPassArgument"
  setPassArgument ::
    Ptr CChar -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenClearPassArguments"
  clearPassArguments ::
    IO ()

foreign import ccall unsafe "BinaryenGetAlwaysInlineMaxSize"
  getAlwaysInlineMaxSize ::
    IO Index

foreign import ccall unsafe "BinaryenSetAlwaysInlineMaxSize"
  setAlwaysInlineMaxSize ::
    Index -> IO ()

foreign import ccall unsafe "BinaryenGetFlexibleInlineMaxSize"
  getFlexibleInlineMaxSize ::
    IO Index

foreign import ccall unsafe "BinaryenSetFlexibleInlineMaxSize"
  setFlexibleInlineMaxSize ::
    Index -> IO ()

foreign import ccall unsafe "BinaryenGetOneCallerInlineMaxSize"
  getOneCallerInlineMaxSize ::
    IO Index

foreign import ccall unsafe "BinaryenSetOneCallerInlineMaxSize"
  setOneCallerInlineMaxSize ::
    Index -> IO ()

foreign import ccall unsafe "BinaryenModuleRunPasses"
  moduleRunPasses ::
    Module -> Ptr (Ptr CChar) -> Index -> IO ()

foreign import ccall unsafe "BinaryenModuleAutoDrop"
  moduleAutoDrop ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModuleWrite"
  moduleWrite ::
    Module -> Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "BinaryenModuleWriteText"
  moduleWriteText ::
    Module -> Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "BinaryenModuleAllocateAndWriteMut"
  moduleAllocateAndWriteMut ::
    Module ->
    Ptr CChar ->
    Ptr (Ptr ()) ->
    Ptr CSize ->
    Ptr (Ptr CChar) ->
    IO ()

foreign import ccall unsafe "BinaryenModuleAllocateAndWriteText"
  moduleAllocateAndWriteText ::
    Module -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenModuleRead"
  moduleRead ::
    Ptr CChar -> CSize -> IO Module

foreign import ccall unsafe "BinaryenModuleInterpret"
  moduleInterpret ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModuleAddDebugInfoFileName"
  moduleAddDebugInfoFileName ::
    Module -> Ptr CChar -> IO Index

foreign import ccall unsafe "BinaryenModuleGetDebugInfoFileName"
  moduleGetDebugInfoFileName ::
    Module -> Index -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionGetName"
  functionGetName ::
    Function -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionGetParams"
  functionGetParams ::
    Function -> IO Type

foreign import ccall unsafe "BinaryenFunctionGetResults"
  functionGetResults ::
    Function -> IO Type

foreign import ccall unsafe "BinaryenFunctionGetNumVars"
  functionGetNumVars ::
    Function -> IO Index

foreign import ccall unsafe "BinaryenFunctionGetVar"
  functionGetVar ::
    Function -> Index -> IO Type

foreign import ccall unsafe "BinaryenFunctionGetBody"
  functionGetBody ::
    Function -> IO Expression

foreign import ccall unsafe "BinaryenFunctionOptimize"
  functionOptimize ::
    Function -> Module -> IO ()

foreign import ccall unsafe "BinaryenFunctionRunPasses"
  functionRunPasses ::
    Function ->
    Module ->
    Ptr (Ptr CChar) ->
    Index ->
    IO ()

foreign import ccall unsafe "BinaryenFunctionSetDebugLocation"
  functionSetDebugLocation ::
    Function ->
    Expression ->
    Index ->
    Index ->
    Index ->
    IO ()

foreign import ccall unsafe "BinaryenGlobalGetName"
  globalGetName ::
    Global -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalGetType"
  globalGetType ::
    Global -> IO Type

foreign import ccall unsafe "BinaryenGlobalIsMutable"
  globalIsMutable ::
    Global -> IO CInt

foreign import ccall unsafe "BinaryenGlobalGetInitExpr"
  globalGetInitExpr ::
    Global -> IO Expression

foreign import ccall unsafe "BinaryenEventGetName"
  eventGetName ::
    Event -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenEventGetAttribute"
  eventGetAttribute ::
    Event -> IO CInt

foreign import ccall unsafe "BinaryenEventGetParams"
  eventGetParams ::
    Event -> IO Type

foreign import ccall unsafe "BinaryenEventGetResults"
  eventGetResults ::
    Event -> IO Type

foreign import ccall unsafe "BinaryenFunctionImportGetModule"
  functionImportGetModule ::
    Function -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalImportGetModule"
  globalImportGetModule ::
    Global -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenEventImportGetModule"
  eventImportGetModule ::
    Event -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionImportGetBase"
  functionImportGetBase ::
    Function -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalImportGetBase"
  globalImportGetBase ::
    Global -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenEventImportGetBase"
  eventImportGetBase ::
    Event -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenExportGetKind"
  exportGetKind ::
    Export -> IO ExternalKind

foreign import ccall unsafe "BinaryenExportGetName"
  exportGetName ::
    Export -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenExportGetValue"
  exportGetValue ::
    Export -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGetNumExports"
  getNumExports ::
    Module -> IO Word32

foreign import ccall unsafe "BinaryenGetExportByIndex"
  getExportByIndex ::
    Module -> Index -> IO Export

foreign import ccall unsafe "BinaryenAddCustomSection"
  addCustomSection ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Index ->
    IO ()

type SideEffects = Word32

foreign import ccall unsafe "BinaryenSideEffectNone" sideEffectNone :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectBranches" sideEffectBranches :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectCalls" sideEffectCalls :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectReadsLocal" sideEffectReadsLocal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectWritesLocal" sideEffectWritesLocal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectReadsGlobal" sideEffectReadsGlobal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectWritesGlobal" sideEffectWritesGlobal :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectReadsMemory" sideEffectReadsMemory :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectWritesMemory" sideEffectWritesMemory :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectImplicitTrap" sideEffectImplicitTrap :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectIsAtomic" sideEffectIsAtomic :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectThrows" sideEffectThrows :: SideEffects
foreign import ccall unsafe "BinaryenSideEffectAny" sideEffectAny :: SideEffects

foreign import ccall unsafe "BinaryenExpressionGetSideEffects"
  expressionGetSideEffects ::
    Expression -> Features -> IO SideEffects

newtype Relooper = Relooper (Ptr Relooper)
  deriving newtype (Eq, Show, Storable)

newtype RelooperBlock = RelooperBlock (Ptr RelooperBlock)
  deriving newtype (Eq, Show, Storable)

foreign import ccall unsafe "RelooperCreate"
  c_RelooperCreate ::
    Module -> IO Relooper

foreign import ccall unsafe "RelooperAddBlock"
  c_RelooperAddBlock ::
    Relooper -> Expression -> IO RelooperBlock

foreign import ccall unsafe "RelooperAddBranch"
  c_RelooperAddBranch ::
    RelooperBlock ->
    RelooperBlock ->
    Expression ->
    Expression ->
    IO ()

foreign import ccall unsafe "RelooperAddBlockWithSwitch"
  c_RelooperAddBlockWithSwitch ::
    Relooper ->
    Expression ->
    Expression ->
    IO RelooperBlock

foreign import ccall unsafe "RelooperAddBranchForSwitch"
  c_RelooperAddBranchForSwitch ::
    RelooperBlock ->
    RelooperBlock ->
    Ptr Index ->
    Index ->
    Expression ->
    IO ()

foreign import ccall unsafe "RelooperRenderAndDispose"
  c_RelooperRenderAndDispose ::
    Relooper ->
    RelooperBlock ->
    Index ->
    IO Expression

foreign import ccall unsafe "BinaryenSetAPITracing"
  setAPITracing ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenSetColorsEnabled"
  setColorsEnabled ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenAreColorsEnabled"
  areColorsEnabled ::
    IO CInt
