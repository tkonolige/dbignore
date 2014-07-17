{-
The C algorithm does not appear to give notable performance
improvements, at least when building tries based on /usr/dict on
little-endian 32-bit machines. The implementation also appears
somewhat buggy (cf test/TrieFile.hs) and using the FFI complicates
distribution.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# CFILES ByteStringInternal/indexOfDifference.c #-}
-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.02.06
-- |
-- Module      :  Data.Trie.ByteStringInternal
-- Copyright   :  Copyright (c) 2008--2011 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Helper functions on 'ByteString's for "Data.Trie.Internal".
----------------------------------------------------------------


module Data.Trie.ByteStringInternal
    ( ByteString, ByteStringElem
    , breakMaximalPrefix
    ) where

import qualified Data.ByteString as S
import Data.ByteString.Internal (ByteString(..), inlinePerformIO)
import Data.Word

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import Foreign.Storable   (Storable(..))

{-
#ifdef __USE_C_INTERNAL__
import Foreign.C.Types (CInt)
import Control.Monad   (liftM)
#endif
-}

----------------------------------------------------------------

-- | Associated type of 'ByteString'
type ByteStringElem = Word8 


----------------------------------------------------------------
-- | Returns the longest shared prefix and the two remaining suffixes
-- for a pair of strings.
--
-- >    s == (\(pre,s',z') -> pre `append` s') (breakMaximalPrefix s z)
-- >    z == (\(pre,s',z') -> pre `append` z') (breakMaximalPrefix s z)

breakMaximalPrefix :: ByteString -> ByteString
                   -> (ByteString, ByteString, ByteString)
breakMaximalPrefix
    str1@(PS s1 off1 len1)
    str2@(PS s2 off2 len2)
    | len1 == 0 = (S.empty, S.empty, str2)
    | len2 == 0 = (S.empty, str1, S.empty)
    | otherwise = inlinePerformIO $
        withForeignPtr s1 $ \p1 ->
        withForeignPtr s2 $ \p2 -> do
            i <- indexOfDifference
                    (p1 `ptrElemOff` off1)
                    (p2 `ptrElemOff` off2)
                    (min len1 len2)
            let pre = if off1 + len1 < off2 + len2  -- share the smaller one
                      then newPS s1 off1 i
                      else newPS s2 off2 i
            let s1' = newPS s1 (off1 + i) (len1 - i)
            let s2' = newPS s2 (off2 + i) (len2 - i)
            
            return $! (,,) !$ pre !$ s1' !$ s2'

-- | C-style pointer addition, without the liberal type of 'plusPtr'.
ptrElemOff :: Storable a => Ptr a -> Int -> Ptr a
{-# INLINE ptrElemOff #-}
ptrElemOff p i =
    p `plusPtr` (i * sizeOf (undefined `asTypeOf` inlinePerformIO (peek p)))

newPS :: ForeignPtr ByteStringElem -> Int -> Int -> ByteString
{-# INLINE newPS #-}
newPS s o l =
    if l <= 0 then S.empty else PS s o l

-- | fix associativity bug
(!$) :: (a -> b) -> a -> b
{-# INLINE (!$) #-}
(!$)  = ($!)


----------------------------------------------------------------
-- | Calculates the first index where values differ.

indexOfDifference :: Ptr ByteStringElem -> Ptr ByteStringElem -> Int -> IO Int
{-
#ifdef __USE_C_INTERNAL__

indexOfDifference p q i =
    liftM fromIntegral $! c_indexOfDifference p q (fromIntegral i)

-- This could probably be not IO, but the wrapper requires that anyways...
foreign import ccall unsafe "ByteStringInternal/indexOfDifference.h indexOfDifference"
    c_indexOfDifference :: Ptr ByteStringElem -> Ptr ByteStringElem -> CInt -> IO CInt

#else
-}

-- Use the naive algorithm which doesn't depend on architecture details
indexOfDifference p1 p2 limit = goByte 0
    where
    goByte n =
        if   n >= limit
        then return limit
        else do c1 <- peekElemOff p1 n
                c2 <- peekElemOff p2 n
                if c1 == c2
                    then goByte $! n+1
                    else return n
{-
#endif
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
