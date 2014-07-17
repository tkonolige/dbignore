{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.04
-- |
-- Module      :  Data.Trie.ByteStringInternal.Configure
-- Copyright   :  Copyright (c) 2008--2011 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  beta
-- Portability :  portable
--
-- Configuration program for C parameters in indexOfDifference.h
----------------------------------------------------------------


module Main (main) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S (c2w)

import Data.Word             (Word8, Word32)
import Foreign.Ptr           (Ptr, castPtr)
import Foreign.Storable      (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types       (CInt)

import Control.Monad (when)
import System.IO
----------------------------------------------------------------

-- TODO: it would be better to fail back to using the naive algorithm
-- in the event of non-standard architectures
errorContactMaintainer  :: String -> Bool
errorContactMaintainer s = error (s ++ " Contact the Data.Trie maintainer.")

-- cf also the new byteorder package
-- <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/byteorder>
-- It's less efficient, but deals with mixed endianness better.
-- It only does detection though, not the bit-munging we need later.
isLittleEndian :: IO Bool
isLittleEndian = alloca $ \p -> do
    poke p    (0x04030201 :: Word32)
    b <- peek (castPtr p  :: Ptr Word8)
    case b of
        0x01 -> return True
        0x04 -> return False
        _    -> errorContactMaintainer "non-standard endianness detected!"

is32Bit :: IO Bool
is32Bit = case sizeOf (undefined :: CInt) of -- BUG: Not the best test...
              4 -> return True
              8 -> return False
              _ -> errorContactMaintainer "non-standard word size detected!"

is64Bit :: IO Bool
is64Bit = case sizeOf (undefined :: CInt) of -- BUG: Not the best test...
              4 -> return False
              8 -> return True
              _ -> errorContactMaintainer "non-standard word size detected!"

main :: IO ()
main = do
    isLE <- isLittleEndian
    is32 <- is32Bit
    is64 <- is64Bit
    withFile "indexOfDifference.h" WriteMode $ \h -> do
        let printLn = S.hPutStrLn h . S.pack . map S.c2w
        printLn "#ifndef __hDataTrie__"
        printLn "#define __hDataTrie__"
        printLn ""
        when isLE $ printLn "#define __hDataTrie_isLittleEndian__"
        when is32 $ printLn "#define __hDataTrie_Nat32__"
        when is64 $ printLn "#define __hDataTrie_Nat64__"
        printLn ""
        printLn "#ifdef __cplusplus"
        printLn "extern \"C\"{"
        printLn "#endif"
        printLn "int indexOfDifference(const void* p1, const void* p2, const int limit);"
        printLn "#ifdef __cplusplus"
        printLn "}"
        printLn "#endif"
        printLn ""
        printLn "#endif /* __hDataTrie__ */"

----------------------------------------------------------------
----------------------------------------------------------- fin.
