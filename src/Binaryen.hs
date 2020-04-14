-- | Global Binaryen state.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

module Binaryen where

import Binaryen.Index
import Binaryen.Event
import Binaryen.Global
import Foreign (Ptr)
import Foreign.C (CChar(..), CInt(..))

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

foreign import ccall unsafe "BinaryenSetAPITracing"
  setAPITracing ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenSetColorsEnabled"
  setColorsEnabled ::
    CInt -> IO ()

foreign import ccall unsafe "BinaryenAreColorsEnabled"
  areColorsEnabled ::
    IO CInt
