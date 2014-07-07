{-# LANGUAGE ForeignFunctionInterface #-}

module Ignore where

import Foreign.C.Types
import Foreign.C.String

import System.Directory
import System.Posix.FilePath

import Data.ByteString

import Data.Trie

import Control.Monad

ignore :: RawFilePath -> Bool
ignore file = False

boolToCInt :: Bool -> CInt
boolToCInt b = case b of
                 True  -> 1
                 False -> 0

ignore_hs :: CString -> IO CInt
ignore_hs str = packCString str >>= return . boolToCInt . ignore

foreign export ccall ignore_hs :: CString -> IO CInt
