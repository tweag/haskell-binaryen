-- | Events.
--
-- See <https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h>
-- for API documentation.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Binaryen.Event where

import Binaryen.Type
import Foreign (Ptr, Storable)
import Foreign.C (CChar(..), CInt(..))

newtype Event = Event (Ptr Event)
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "BinaryenEventGetName"
  getName ::
    Event -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenEventGetAttribute"
  getAttribute ::
    Event -> IO CInt

foreign import ccall unsafe "BinaryenEventGetParams"
  getParams ::
    Event -> IO Type

foreign import ccall unsafe "BinaryenEventGetResults"
  getResults ::
    Event -> IO Type

foreign import ccall unsafe "BinaryenEventImportGetModule"
  importGetModule ::
    Event -> IO (Ptr CChar)
