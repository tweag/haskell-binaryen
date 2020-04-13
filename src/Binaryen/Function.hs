-- | Functions.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Function where

import Binaryen.Expression
import Binaryen.Index
import {-# SOURCE #-} Binaryen.Module
import Binaryen.Type
import Foreign (Ptr, Storable)
import Foreign.C (CChar(..))

newtype Function = Function (Ptr Function)
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenFunctionGetName"
  getName ::
    Function -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionGetParams"
  getParams ::
    Function -> IO Type

foreign import ccall unsafe "BinaryenFunctionGetResults"
  getResults ::
    Function -> IO Type

foreign import ccall unsafe "BinaryenFunctionGetNumVars"
  getNumVars ::
    Function -> IO Index

foreign import ccall unsafe "BinaryenFunctionGetVar"
  getVar ::
    Function -> Index -> IO Type

foreign import ccall unsafe "BinaryenFunctionGetBody"
  getBody ::
    Function -> IO Expression

foreign import ccall unsafe "BinaryenFunctionSetDebugLocation"
  setDebugLocation ::
    Function ->
    Expression ->
    Index ->
    Index ->
    Index ->
    IO ()

foreign import ccall unsafe "BinaryenFunctionImportGetModule"
  importGetModule ::
    Function -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionImportGetBase"
  importGetBase ::
    Function -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionOptimize"
  optimize ::
    Function -> Module -> IO ()

foreign import ccall unsafe "BinaryenFunctionRunPasses"
  runPasses ::
    Function ->
    Module ->
    Ptr (Ptr CChar) ->
    Index ->
    IO ()
