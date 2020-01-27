{-# LANGUAGE PolyKinds #-}

module Binaryen.IR
  ( Index (..),
    Type (..),
    Feature (..),
    Literal (..),
    Op (..),
    Expression (..),
    Function (..),
    FunctionImport (..),
    TableImport (..),
    MemoryImport (..),
    GlobalImport (..),
    EventImport (..),
    FunctionExport (..),
    TableExport (..),
    MemoryExport (..),
    GlobalExport (..),
    EventExport (..),
    Global (..),
    Event (..),
    FunctionTable (..),
    Memory (..)
  )
where

import qualified Data.ByteString as B
import qualified Foreign as F

newtype Index
  = Index F.Word32

data Type f
  = None
  | Int32
  | Int64
  | Float32
  | Float64
  | Vec128
  | Funcref
  | Anyref
  | Nullref
  | Exnref
  | UnreachableType
  | Auto
  | MultiValue [f (Type f)]

data Feature f
  = MVP
  | Atomics
  | BulkMemory
  | MutableGlobals
  | NontrappingFPToInt
  | SignExt
  | SIMD128
  | ExceptionHandling
  | TailCall
  | ReferenceTypes
  | All

data Literal f
  = LiteralInt32 !F.Int32
  | LiteralInt64 !F.Int64
  | LiteralFloat32 !Float
  | LiteralFloat64 !Double
  | LiteralVec128 !B.ByteString
  | LiteralFloat32Bits !F.Int32
  | LiteralFloat64Bits !F.Int64

data Op f
  = ClzInt32
  | CtzInt32
  | PopcntInt32
  | NegFloat32
  | AbsFloat32
  | CeilFloat32
  | FloorFloat32
  | TruncFloat32
  | NearestFloat32
  | SqrtFloat32
  | EqZInt32
  | ClzInt64
  | CtzInt64
  | PopcntInt64
  | NegFloat64
  | AbsFloat64
  | CeilFloat64
  | FloorFloat64
  | TruncFloat64
  | NearestFloat64
  | SqrtFloat64
  | EqZInt64
  | ExtendSInt32
  | ExtendUInt32
  | WrapInt64
  | TruncSFloat32ToInt32
  | TruncSFloat32ToInt64
  | TruncUFloat32ToInt32
  | TruncUFloat32ToInt64
  | TruncSFloat64ToInt32
  | TruncSFloat64ToInt64
  | TruncUFloat64ToInt32
  | TruncUFloat64ToInt64
  | ReinterpretFloat32
  | ReinterpretFloat64
  | ConvertSInt32ToFloat32
  | ConvertSInt32ToFloat64
  | ConvertUInt32ToFloat32
  | ConvertUInt32ToFloat64
  | ConvertSInt64ToFloat32
  | ConvertSInt64ToFloat64
  | ConvertUInt64ToFloat32
  | ConvertUInt64ToFloat64
  | PromoteFloat32
  | DemoteFloat64
  | ReinterpretInt32
  | ReinterpretInt64
  | ExtendS8Int32
  | ExtendS16Int32
  | ExtendS8Int64
  | ExtendS16Int64
  | ExtendS32Int64
  | AddInt32
  | SubInt32
  | MulInt32
  | DivSInt32
  | DivUInt32
  | RemSInt32
  | RemUInt32
  | AndInt32
  | OrInt32
  | XorInt32
  | ShlInt32
  | ShrUInt32
  | ShrSInt32
  | RotLInt32
  | RotRInt32
  | EqInt32
  | NeInt32
  | LtSInt32
  | LtUInt32
  | LeSInt32
  | LeUInt32
  | GtSInt32
  | GtUInt32
  | GeSInt32
  | GeUInt32
  | AddInt64
  | SubInt64
  | MulInt64
  | DivSInt64
  | DivUInt64
  | RemSInt64
  | RemUInt64
  | AndInt64
  | OrInt64
  | XorInt64
  | ShlInt64
  | ShrUInt64
  | ShrSInt64
  | RotLInt64
  | RotRInt64
  | EqInt64
  | NeInt64
  | LtSInt64
  | LtUInt64
  | LeSInt64
  | LeUInt64
  | GtSInt64
  | GtUInt64
  | GeSInt64
  | GeUInt64
  | AddFloat32
  | SubFloat32
  | MulFloat32
  | DivFloat32
  | CopySignFloat32
  | MinFloat32
  | MaxFloat32
  | EqFloat32
  | NeFloat32
  | LtFloat32
  | LeFloat32
  | GtFloat32
  | GeFloat32
  | AddFloat64
  | SubFloat64
  | MulFloat64
  | DivFloat64
  | CopySignFloat64
  | MinFloat64
  | MaxFloat64
  | EqFloat64
  | NeFloat64
  | LtFloat64
  | LeFloat64
  | GtFloat64
  | GeFloat64
  | MemorySize
  | MemoryGrow
  | AtomicRMWAdd
  | AtomicRMWSub
  | AtomicRMWAnd
  | AtomicRMWOr
  | AtomicRMWXor
  | AtomicRMWXchg
  | TruncSatSFloat32ToInt32
  | TruncSatSFloat32ToInt64
  | TruncSatUFloat32ToInt32
  | TruncSatUFloat32ToInt64
  | TruncSatSFloat64ToInt32
  | TruncSatSFloat64ToInt64
  | TruncSatUFloat64ToInt32
  | TruncSatUFloat64ToInt64
  | SplatVecI8x16
  | ExtractLaneSVecI8x16
  | ExtractLaneUVecI8x16
  | ReplaceLaneVecI8x16
  | SplatVecI16x8
  | ExtractLaneSVecI16x8
  | ExtractLaneUVecI16x8
  | ReplaceLaneVecI16x8
  | SplatVecI32x4
  | ExtractLaneVecI32x4
  | ReplaceLaneVecI32x4
  | SplatVecI64x2
  | ExtractLaneVecI64x2
  | ReplaceLaneVecI64x2
  | SplatVecF32x4
  | ExtractLaneVecF32x4
  | ReplaceLaneVecF32x4
  | SplatVecF64x2
  | ExtractLaneVecF64x2
  | ReplaceLaneVecF64x2
  | EqVecI8x16
  | NeVecI8x16
  | LtSVecI8x16
  | LtUVecI8x16
  | GtSVecI8x16
  | GtUVecI8x16
  | LeSVecI8x16
  | LeUVecI8x16
  | GeSVecI8x16
  | GeUVecI8x16
  | EqVecI16x8
  | NeVecI16x8
  | LtSVecI16x8
  | LtUVecI16x8
  | GtSVecI16x8
  | GtUVecI16x8
  | LeSVecI16x8
  | LeUVecI16x8
  | GeSVecI16x8
  | GeUVecI16x8
  | EqVecI32x4
  | NeVecI32x4
  | LtSVecI32x4
  | LtUVecI32x4
  | GtSVecI32x4
  | GtUVecI32x4
  | LeSVecI32x4
  | LeUVecI32x4
  | GeSVecI32x4
  | GeUVecI32x4
  | EqVecF32x4
  | NeVecF32x4
  | LtVecF32x4
  | GtVecF32x4
  | LeVecF32x4
  | GeVecF32x4
  | EqVecF64x2
  | NeVecF64x2
  | LtVecF64x2
  | GtVecF64x2
  | LeVecF64x2
  | GeVecF64x2
  | NotVec128
  | AndVec128
  | OrVec128
  | XorVec128
  | AndNotVec128
  | BitselectVec128
  | NegVecI8x16
  | AnyTrueVecI8x16
  | AllTrueVecI8x16
  | ShlVecI8x16
  | ShrSVecI8x16
  | ShrUVecI8x16
  | AddVecI8x16
  | AddSatSVecI8x16
  | AddSatUVecI8x16
  | SubVecI8x16
  | SubSatSVecI8x16
  | SubSatUVecI8x16
  | MulVecI8x16
  | MinSVecI8x16
  | MinUVecI8x16
  | MaxSVecI8x16
  | MaxUVecI8x16
  | AvgrUVecI8x16
  | NegVecI16x8
  | AnyTrueVecI16x8
  | AllTrueVecI16x8
  | ShlVecI16x8
  | ShrSVecI16x8
  | ShrUVecI16x8
  | AddVecI16x8
  | AddSatSVecI16x8
  | AddSatUVecI16x8
  | SubVecI16x8
  | SubSatSVecI16x8
  | SubSatUVecI16x8
  | MulVecI16x8
  | MinSVecI16x8
  | MinUVecI16x8
  | MaxSVecI16x8
  | MaxUVecI16x8
  | AvgrUVecI16x8
  | NegVecI32x4
  | AnyTrueVecI32x4
  | AllTrueVecI32x4
  | ShlVecI32x4
  | ShrSVecI32x4
  | ShrUVecI32x4
  | AddVecI32x4
  | SubVecI32x4
  | MulVecI32x4
  | MinSVecI32x4
  | MinUVecI32x4
  | MaxSVecI32x4
  | MaxUVecI32x4
  | DotSVecI16x8ToVecI32x4
  | NegVecI64x2
  | AnyTrueVecI64x2
  | AllTrueVecI64x2
  | ShlVecI64x2
  | ShrSVecI64x2
  | ShrUVecI64x2
  | AddVecI64x2
  | SubVecI64x2
  | AbsVecF32x4
  | NegVecF32x4
  | SqrtVecF32x4
  | QFMAVecF32x4
  | QFMSVecF32x4
  | AddVecF32x4
  | SubVecF32x4
  | MulVecF32x4
  | DivVecF32x4
  | MinVecF32x4
  | MaxVecF32x4
  | AbsVecF64x2
  | NegVecF64x2
  | SqrtVecF64x2
  | QFMAVecF64x2
  | QFMSVecF64x2
  | AddVecF64x2
  | SubVecF64x2
  | MulVecF64x2
  | DivVecF64x2
  | MinVecF64x2
  | MaxVecF64x2
  | TruncSatSVecF32x4ToVecI32x4
  | TruncSatUVecF32x4ToVecI32x4
  | TruncSatSVecF64x2ToVecI64x2
  | TruncSatUVecF64x2ToVecI64x2
  | ConvertSVecI32x4ToVecF32x4
  | ConvertUVecI32x4ToVecF32x4
  | ConvertSVecI64x2ToVecF64x2
  | ConvertUVecI64x2ToVecF64x2
  | LoadSplatVec8x16
  | LoadSplatVec16x8
  | LoadSplatVec32x4
  | LoadSplatVec64x2
  | LoadExtSVec8x8ToVecI16x8
  | LoadExtUVec8x8ToVecI16x8
  | LoadExtSVec16x4ToVecI32x4
  | LoadExtUVec16x4ToVecI32x4
  | LoadExtSVec32x2ToVecI64x2
  | LoadExtUVec32x2ToVecI64x2
  | NarrowSVecI16x8ToVecI8x16
  | NarrowUVecI16x8ToVecI8x16
  | NarrowSVecI32x4ToVecI16x8
  | NarrowUVecI32x4ToVecI16x8
  | WidenLowSVecI8x16ToVecI16x8
  | WidenHighSVecI8x16ToVecI16x8
  | WidenLowUVecI8x16ToVecI16x8
  | WidenHighUVecI8x16ToVecI16x8
  | WidenLowSVecI16x8ToVecI32x4
  | WidenHighSVecI16x8ToVecI32x4
  | WidenLowUVecI16x8ToVecI32x4
  | WidenHighUVecI16x8ToVecI32x4
  | SwizzleVec8x16

data Expression f
  = Block
      { blockName :: !B.ByteString,
        blockChildren :: [f (Expression f)],
        blockType :: f (Type f)
      }
  | If
      { ifCondition, ifTrue :: f (Expression f),
        ifFalse :: Maybe (f (Expression f))
      }
  | Loop
      { loopName :: !B.ByteString,
        loopBody :: f (Expression f)
      }
  | Break
      { breakName :: !B.ByteString,
        breakCondition, breakValue :: Maybe (f (Expression f))
      }
  | Switch
      { switchNames :: [B.ByteString],
        switchDefaultName :: !B.ByteString,
        switchCondition :: f (Expression f),
        switchValue :: Maybe (f (Expression f))
      }
  | Call
      { callTarget :: !B.ByteString,
        callOperands :: [f (Expression f)],
        callReturnType :: f (Type f)
      }
  | CallIndirect
      { callIndirectTarget :: f (Expression f),
        callIndirectOperands :: [f (Expression f)],
        callIndirectParamType, callIndirectResultType :: f (Type f)
      }
  | ReturnCall
      { returnCallTarget :: !B.ByteString,
        returnCallOperands :: [f (Expression f)],
        returnCallReturnType :: f (Type f)
      }
  | ReturnCallIndirect
      { returnCallIndirectTarget :: f (Expression f),
        returnCallIndirectOperands :: [f (Expression f)],
        returnCallIndirectParamType, returnCallIndirectResultType :: f (Type f)
      }
  | LocalGet
      { localGetIndex :: !Index,
        localGetType :: f (Type f)
      }
  | LocalSet
      { localSetIndex :: !Index,
        localSetValue :: f (Expression f)
      }
  | LocalTee
      { localTeeIndex :: !Index,
        localTeeValue :: f (Expression f),
        localTeeType :: f (Type f)
      }
  | GlobalGet
      { globalGetName :: !B.ByteString,
        globalGetType :: f (Type f)
      }
  | GlobalSet
      { globalSetName :: !B.ByteString,
        globalSetValue :: f (Expression f)
      }
  | Load
      { loadBytes :: !F.Word32,
        loadSigned :: !Bool,
        loadOffset, loadAlign :: !F.Word32,
        loadType :: f (Type f),
        loadPtr :: f (Expression f)
      }
  | Store
      { storeBytes, storeOffset, storeAlign :: !F.Word32,
        storePtr, storeValue :: f (Expression f),
        storeType :: f (Type f)
      }
  | Const
      { constValue :: f (Literal f)
      }
  | Unary
      { unaryOp :: f (Op f),
        unaryValue :: f (Expression f)
      }
  | Binary
      { binaryOp :: f (Op f),
        binaryLeft, binaryRight :: f (Expression f)
      }
  | Select
      { selectCondition, selectIfTrue, selectIfFalse :: f (Expression f),
        selectType :: f (Type f)
      }
  | Drop
      { dropValue :: f (Expression f)
      }
  | Return
      { returnValue :: Maybe (f (Expression f))
      }
  | Host
      { hostOp :: f (Op f),
        hostName :: !B.ByteString,
        hostOperands :: [f (Expression f)]
      }
  | Nop
  | Unreachable
  | AtomicLoad
      { atomicLoadBytes, atomicLoadOffset :: !F.Word32,
        atomicLoadType :: f (Type f),
        atomicLoadPtr :: f (Expression f)
      }
  | AtomicStore
      { atomicStoreBytes, atomicStoreOffset :: !F.Word32,
        atomicStorePtr, atomicStoreValue :: f (Expression f),
        atomicStoreType :: f (Type f)
      }
  | AtomicRMW
      { atomicRMWOp :: f (Op f),
        atomicRMW, atomicRMWOffset :: !F.Word32,
        atomicRMWPtr, atomicRMWValue :: f (Expression f),
        atomicRMWType :: f (Type f)
      }
  | AtomicCmpxchg
      { atomicCmpxchgBytes, atomicCmpxchgOffset :: !F.Word32,
        atomicCmpxchgPtr, atomicCmpxchgExpected, atomicCmpxchgReplacement :: f (Expression f),
        atomicCmpxchgType :: f (Type f)
      }
  | AtomicWait
      { atomicWaitPtr, atomicWaitExpected, atomicWaitTimeout :: f (Expression f),
        atomicWaitType :: f (Type f)
      }
  | AtomicNotify
      { atomicNotifyPtr, atomicNotifyCount :: f (Expression f)
      }
  | AtomicFence
  | SIMDExtract
      { simdExtractOp :: f (Op f),
        simdExtractVec :: f (Expression f),
        simdExtractIndex :: !F.Word8
      }
  | SIMDReplace
      { simdReplaceOp :: f (Op f),
        simdReplaceVec :: f (Expression f),
        simdReplaceIndex :: !F.Word8,
        simdReplaceValue :: f (Expression f)
      }
  | SIMDShuffle
      { simdShuffleLeft, simdShuffleRight :: f (Expression f),
        simdShuffleMask :: !B.ByteString
      }
  | SIMDTernary
      { simdTernaryOp :: f (Op f),
        simdTernaryA, simdTernaryB, simdTernaryC :: f (Expression f)
      }
  | SIMDShift
      { simdShiftOp :: f (Op f),
        simdShiftVec, simdShiftShift :: f (Expression f)
      }
  | SIMDLoad
      { simdLoadOp :: f (Op f),
        simdLoadOffset, simdLoadAlign :: !F.Word32,
        simdLoadPtr :: f (Expression f)
      }
  | MemoryInit
      { memoryInitSegment :: !F.Word32,
        memoryInitDest, memoryInitOffset, memoryInitSize :: f (Expression f)
      }
  | DataDrop
      { dataDropSegment :: !F.Word32
      }
  | MemoryCopy
      { memoryCopyDest, memoryCopySource, memoryCopySize :: f (Expression f)
      }
  | MemoryFill
      { memoryFillDest, memoryFillValue, memoryFillSize :: f (Expression f)
      }
  | RefNull
  | RefIsNull
      { refIsNullValue :: f (Expression f)
      }
  | RefFunc
      { refFunc :: !B.ByteString
      }
  | Try
      { tryBody, tryCatchBody :: f (Expression f)
      }
  | Throw
      { throwEvent :: !B.ByteString,
        throwOperands :: [f (Expression f)]
      }
  | Rethrow
      { rethrowExnref :: f (Expression f)
      }
  | BrOnExn
      { brOnExnName, brOnExnEventName :: !B.ByteString,
        brOnExnExnref :: f (Expression f)
      }
  | Push
      { pushValue :: f (Expression f)
      }
  | Pop
      { popType :: f (Type f)
      }

data Function f
  = Function
      { functionName :: !B.ByteString,
        functionParamType, functionResultType :: f (Type f),
        functionVarTypes :: [f (Type f)],
        functionBody :: f (Expression f)
      }

data FunctionImport f
  = FunctionImport
      { functionImportInternalName, functionImportExternalModuleName, functionImportExternalBaseName :: !B.ByteString,
        functionImportParamType, functionImportResultType :: f (Type f)
      }

data TableImport f
  = TableImport
      { tableImportInternalName, tableImportExternalModuleName, tableImportExternalBaseName :: !B.ByteString
      }

data MemoryImport f
  = MemoryImport
      { memoryImportInternalName, memoryImportExternalModuleName, memoryImportExternalBaseName :: !B.ByteString,
        memoryImportShared :: !Bool
      }

data GlobalImport f
  = GlobalImport
      { globalImportInternalName, globalImportExternalModuleName, globalImportExternalBaseName :: !B.ByteString,
        globalImportType :: f (Type f),
        globalImportMutable :: !Bool
      }

data EventImport f
  = EventImport
      { eventImportInternalName, eventImportExternalModuleName, eventImportExternalBaseName :: !B.ByteString,
        eventImportAttribute :: !F.Word32,
        eventImportParamType, eventImportResultType :: f (Type f)
      }

data FunctionExport f
  = FunctionExport
      { functionExportInternalName, functionExportExternalName :: !B.ByteString
      }

data TableExport f
  = TableExport
      { tableExportInternalName, tableExportExternalName :: !B.ByteString
      }

data MemoryExport f
  = MemoryExport
      { memoryExportInternalName, memoryExportExternalName :: !B.ByteString
      }

data GlobalExport f
  = GlobalExport
      { globalExportInternalName, globalExportExternalName :: !B.ByteString
      }

data EventExport f
  = EventExport
      { eventExportInternalName, eventExportExternalName :: !B.ByteString
      }

data Global f
  = Global
      { globalName :: !B.ByteString,
        globalType :: f (Type f),
        globalMutable :: !Bool,
        globalInit :: f (Expression f)
      }

data Event f
  = Event
      { eventName :: !B.ByteString,
        eventAttribute :: !F.Word32,
        eventParamType, eventResultType :: f (Type f)
      }

data FunctionTable f
  = FunctionTable
      { functionTableInitial, functionTableMaximum :: !F.Word32,
        functionTableNames :: [B.ByteString],
        functionTableOffset :: f (Expression f)
      }

data Memory f = Memory {
  memoryInitial, memoryMaximum :: !F.Word32,
  memoryExportName :: Maybe B.ByteString
}
