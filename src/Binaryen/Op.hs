{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Operators.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.
module Binaryen.Op where

import Data.Int (Int32)
import Foreign (Storable)

newtype Op = Op Int32
  deriving (Eq, Show, Storable)

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
