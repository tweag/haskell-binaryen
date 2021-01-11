{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Modules.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.
--
-- This module is intended to be imported qualified.
module Binaryen.Module where

import Binaryen.Event
import Binaryen.Export
import Binaryen.Expression
import Binaryen.Features
import Binaryen.Function
import Binaryen.Global
import Binaryen.Index
import Binaryen.Type
import Data.Int (Int8)
import Data.Word
  ( Word32,
    Word8,
  )
import Foreign
  ( Ptr,
    Storable,
  )
import Foreign.C
  ( CChar (..),
    CInt (..),
    CSize (..),
    CUIntPtr (..),
  )

newtype Module = Module (Ptr Module)
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenModuleCreate"
  create ::
    IO Module

foreign import ccall unsafe "BinaryenModuleDispose"
  dispose ::
    Module -> IO ()

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
    Module -> IO Index

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
  getFeatures ::
    Module -> IO Features

foreign import ccall unsafe "BinaryenModuleSetFeatures"
  setFeatures ::
    Module -> Features -> IO ()

foreign import ccall unsafe "BinaryenModuleParse"
  parse ::
    Ptr CChar -> IO Module

foreign import ccall unsafe "BinaryenModulePrint"
  print ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModulePrintAsmjs"
  printAsmjs ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModuleValidate"
  validate ::
    Module -> IO CInt

foreign import ccall unsafe "BinaryenModuleOptimize"
  optimize ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenAddCustomSection"
  addCustomSection ::
    Module ->
    Ptr CChar ->
    Ptr CChar ->
    Index ->
    IO ()

foreign import ccall unsafe "BinaryenGetNumExports"
  getNumExports ::
    Module -> IO Index

foreign import ccall unsafe "BinaryenGetExportByIndex"
  getExportByIndex ::
    Module -> Index -> IO Export

foreign import ccall unsafe "BinaryenModuleRunPasses"
  runPasses ::
    Module -> Ptr (Ptr CChar) -> Index -> IO ()

foreign import ccall unsafe "BinaryenModuleAutoDrop"
  autoDrop ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModuleWrite"
  write ::
    Module -> Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "BinaryenModuleWriteText"
  writeText ::
    Module -> Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "BinaryenModuleAllocateAndWriteMut"
  allocateAndWriteMut ::
    Module ->
    Ptr CChar ->
    Ptr (Ptr ()) ->
    Ptr CSize ->
    Ptr (Ptr CChar) ->
    IO ()

foreign import ccall unsafe "BinaryenModuleAllocateAndWriteText"
  allocateAndWriteText ::
    Module -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenModuleRead"
  read ::
    Ptr CChar -> CSize -> IO Module

foreign import ccall unsafe "BinaryenModuleInterpret"
  interpret ::
    Module -> IO ()

foreign import ccall unsafe "BinaryenModuleAddDebugInfoFileName"
  addDebugInfoFileName ::
    Module -> Ptr CChar -> IO Index

foreign import ccall unsafe "BinaryenModuleGetDebugInfoFileName"
  getDebugInfoFileName ::
    Module -> Index -> IO (Ptr CChar)

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
