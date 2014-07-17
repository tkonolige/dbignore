{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-name-shadowing #-}

-- The MagicHash is for unboxed primitives (-fglasgow-exts also works)
{-# LANGUAGE CPP, MagicHash #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.05
-- |
-- Module      :  Data.Trie.BitTwiddle
-- Copyright   :  Copyright (c) 2002 Daan Leijen
-- License     :  BSD3
-- Maintainer  :  libraries@haskell.org, wren@community.haskell.org
-- Stability   :  stable
-- Portability :  portable (with CPP)
--
-- Functions to treat 'Word' as a bit-vector for big-endian patricia
-- trees. This code is duplicated from "Data.IntMap". The only
-- differences are that some of the conversion functions are
-- specialized to 'Word8' for bytestrings, instead of being specialized
-- to 'Int'.
----------------------------------------------------------------

module Data.Trie.BitTwiddle
    ( Prefix, Mask
    , elemToNat
    , zero, nomatch
    , mask, shorter, branchMask
    ) where

import Data.Trie.ByteStringInternal (ByteStringElem)

import Data.Bits

#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts  ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import GlaExts   ( Word(..), Int(..), shiftRL# )
#else
import Data.Word (Word)
#endif

----------------------------------------------------------------

type KeyElem = ByteStringElem 
type Prefix  = KeyElem 
type Mask    = KeyElem 

elemToNat :: KeyElem -> Word
{-# INLINE elemToNat #-}
elemToNat = fromIntegral

natToElem :: Word -> KeyElem
{-# INLINE natToElem #-}
natToElem = fromIntegral

shiftRL :: Word -> Int -> Word
{-# INLINE shiftRL #-}
#if __GLASGOW_HASKELL__
-- GHC: use unboxing to get @shiftRL@ inlined.
shiftRL (W# x) (I# i) = W# (shiftRL# x i)
#else
shiftRL x i = shiftR x i
#endif


{---------------------------------------------------------------
-- Endian independent bit twiddling (Trie endianness, not architecture)
---------------------------------------------------------------}

-- TODO: should we use the (Bits Word8) instance instead of 'elemToNat' and (Bits Nat)? We need to compare Core, C--, or ASM in order to decide this. The choice will apply to 'zero', 'mask', 'maskW',... If we shouldn't, then we should probably send a patch upstream to fix the (Bits Word8) instance.

-- | Is the value under the mask zero?
zero :: KeyElem -> Mask -> Bool
{-# INLINE zero #-}
zero i m = (elemToNat i) .&. (elemToNat m) == 0

-- | Does a value /not/ match some prefix, for all the bits preceding
-- a masking bit? (Hence a subtree matching the value doesn't exist.)
nomatch :: KeyElem -> Prefix -> Mask -> Bool
{-# INLINE nomatch #-}
nomatch i p m = mask i m /= p

mask :: KeyElem -> Mask -> Prefix
{-# INLINE mask #-}
mask i m = maskW (elemToNat i) (elemToNat m)


{---------------------------------------------------------------
-- Big endian operations (Trie endianness, not architecture)
---------------------------------------------------------------}

-- | Get mask by setting all bits higher than the smallest bit in
-- @m@. Then apply that mask to @i@.
maskW :: Word -> Word -> Prefix
{-# INLINE maskW #-}
maskW i m = natToElem (i .&. (complement (m-1) `xor` m))
-- TODO: try the alternatives mentioned in the Containers paper:
-- \i m -> natToElem (i .&. (negate m - m))
-- \i m -> natToElem (i .&. (m * complement 1))
-- N.B. these return /all/ the low bits, and therefore they are not equal functions for all m. They are, however, equal when only one bit of m is set.

-- | Determine whether the first mask denotes a shorter prefix than
-- the second.
shorter :: Mask -> Mask -> Bool
{-# INLINE shorter #-}
shorter m1 m2 = elemToNat m1 > elemToNat m2

-- | Determine first differing bit of two prefixes.
branchMask :: Prefix -> Prefix -> Mask
{-# INLINE branchMask #-}
branchMask p1 p2
    = natToElem (highestBitMask (elemToNat p1 `xor` elemToNat p2))

{---------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently
  in three ways:
  * convert to a floating point value and the mantissa tells us the
    [log2(x)] that corresponds with the highest bit position. The
    mantissa is retrieved either via the standard C function [frexp]
    or by some bit twiddling on IEEE compatible numbers (float).
    Note that one needs to use at least [double] precision for an
    accurate mantissa of 32 bit numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and
    shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an
  AMD Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC
  instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even
  faster than a single CISC instruction (BSR)!
---------------------------------------------------------------}

{---------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is
  set. It is found by first setting all bits in lower positions
  than the highest bit and than taking an exclusive or with the
  original value. Allthough the function may look expensive, GHC
  compiles this into excellent C code that subsequently compiled
  into highly efficient machine code. The algorithm is derived from
  Jorg Arndt's FXT library.
---------------------------------------------------------------}
highestBitMask :: Word -> Word
{-# INLINE highestBitMask #-}
highestBitMask x
    = case (x .|. shiftRL x 1) of 
       x -> case (x .|. shiftRL x 2) of 
        x -> case (x .|. shiftRL x 4) of 
         x -> case (x .|. shiftRL x 8) of 
          x -> case (x .|. shiftRL x 16) of 
           x -> case (x .|. shiftRL x 32) of   -- for 64 bit platforms
            x -> (x `xor` shiftRL x 1)

----------------------------------------------------------------
----------------------------------------------------------- fin.
