-- | Control flow graph / relooper.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Relooper where

import Binaryen.Expression
import Binaryen.Index
import Binaryen.Module
import Foreign (Ptr, Storable)

newtype Relooper = Relooper (Ptr Relooper)
  deriving (Eq, Show, Storable)

newtype RelooperBlock = RelooperBlock (Ptr RelooperBlock)
  deriving (Eq, Show)

foreign import ccall unsafe "RelooperCreate"
  create ::
    Module -> IO Relooper

foreign import ccall unsafe "RelooperAddBlock"
  addBlock ::
    Relooper -> Expression -> IO RelooperBlock

foreign import ccall unsafe "RelooperAddBranch"
  addBranch ::
    RelooperBlock ->
    RelooperBlock ->
    Expression ->
    Expression ->
    IO ()

foreign import ccall unsafe "RelooperAddBlockWithSwitch"
  addBlockWithSwitch ::
    Relooper ->
    Expression ->
    Expression ->
    IO RelooperBlock

foreign import ccall unsafe "RelooperAddBranchForSwitch"
  addBranchForSwitch ::
    RelooperBlock ->
    RelooperBlock ->
    Ptr Index ->
    Index ->
    Expression ->
    IO ()

foreign import ccall unsafe "RelooperRenderAndDispose"
  renderAndDispose ::
    Relooper ->
    RelooperBlock ->
    Index ->
    IO Expression
