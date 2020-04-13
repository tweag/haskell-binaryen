module Binaryen.Module where

import Foreign.Ptr (Ptr)

newtype Module = Module (Ptr Module)
