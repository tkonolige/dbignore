{-# LANGUAGE ForeignFunctionInterface #-}

module Ignore where

import Foreign.C.Types
import Foreign.C.String

import System.FilePath

ignore :: FilePath -> Bool
ignore file = False

ignore_hs :: CString -> CInt
ignore_hs str = 0

foreign export ccall ignore_hs :: CString -> CInt
