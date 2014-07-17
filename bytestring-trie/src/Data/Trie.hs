-- To make GHC stop warning about the Prelude
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}
----------------------------------------------------------------
--                                                  ~ 2011.02.12
-- |
-- Module      :  Data.Trie
-- Copyright   :  Copyright (c) 2008--2011 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- An efficient implementation of finite maps from strings to values.
-- The implementation is based on /big-endian patricia trees/, like
-- "Data.IntMap". We first trie on the elements of "Data.ByteString"
-- and then trie on the big-endian bit representation of those
-- elements. For further details on the latter, see
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--    Workshop on ML, September 1998, pages 77-86,
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve/
--    /Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--    October 1968, pages 514-534.
--
-- This module aims to provide an austere interface, while being
-- detailed enough for most users. For an extended interface with
-- many additional functions, see "Data.Trie.Convenience". For
-- functions that give more detailed (potentially abstraction-breaking)
-- access to the data strucuture, or for experimental functions
-- which aren't quite ready for the public API, see "Data.Trie.Internal".
----------------------------------------------------------------

module Data.Trie
    (
    -- * Data type
      Trie()
    
    -- * Basic functions
    , empty, null, singleton, size
    
    -- * Conversion functions
    , fromList, toListBy, toList, keys, elems
    
    -- * Query functions
    , lookupBy, lookup, member, submap, nearestMatch
    
    -- * Single-value modification
    , alterBy, insert, adjust, delete
    
    -- * Combining tries
    , mergeBy, unionL, unionR
    
    -- * Mapping functions
    , mapBy, filterMap
    ) where

import Prelude hiding     (null, lookup)
import qualified Prelude  (null, lookup)

import Data.Trie.Internal
import Data.Trie.Errors   (impossible)
import Data.ByteString    (ByteString)
import Data.Maybe         (isJust)
import Control.Monad      (liftM)
----------------------------------------------------------------
----------------------------------------------------------------


{---------------------------------------------------------------
-- Conversion functions 
---------------------------------------------------------------}

-- | Convert association list into a trie. On key conflict, values
-- earlier in the list shadow later ones.
fromList :: [(ByteString,a)] -> Trie a
{-# INLINE fromList #-}
fromList = foldr (uncurry insert) empty

-- | Convert trie into association list. Keys will be in sorted order.
toList :: Trie a -> [(ByteString,a)]
{-# INLINE toList #-}
toList  = toListBy (,)

-- FIX? should 'keys' and 'elems' move to Data.Trie.Convenience instead?

-- | Return all keys in the trie, in sorted order.
keys :: Trie a -> [ByteString]
{-# INLINE keys #-}
keys  = toListBy const

-- | Return all values in the trie, in sorted order according to the keys.
elems :: Trie a -> [a]
{-# INLINE elems #-}
elems  = toListBy (flip const)


{---------------------------------------------------------------
-- Query functions (just recurse)
---------------------------------------------------------------}

-- | Generic function to find a value (if it exists) and the subtrie
-- rooted at the prefix.
lookupBy :: (Maybe a -> Trie a -> b) -> ByteString -> Trie a -> b
{-# INLINE lookupBy #-}
lookupBy f = lookupBy_ f (f Nothing empty) (f Nothing)

-- | Return the value associated with a query string if it exists.
lookup :: ByteString -> Trie a -> Maybe a
{-# INLINE lookup #-}
lookup = lookupBy_ const Nothing (const Nothing)

-- TODO? move to "Data.Trie.Conventience"?
-- | Does a string have a value in the trie?
member :: ByteString -> Trie a -> Bool
{-# INLINE member #-}
member q = isJust . lookup q

-- | Return closest lookup
nearestMatch :: Show a => ByteString -> Trie a -> Maybe (ByteString, a)
nearestMatch = nearestMatch_


{---------------------------------------------------------------
-- Single-value modification functions (recurse and clone spine)
---------------------------------------------------------------}

-- | Insert a new key. If the key is already present, overrides the
-- old value
insert :: ByteString -> a -> Trie a -> Trie a
{-# INLINE insert #-}
insert = alterBy (\_ x _ -> Just x)

-- | Apply a function to the value at a key.
adjust :: (a -> a) -> ByteString -> Trie a -> Trie a
{-# INLINE adjust #-}
adjust f q = adjustBy (\_ _ -> f) q (impossible "adjust")
-- TODO: benchmark vs the definition with alterBy/liftM

-- | Remove the value stored at a key.
delete :: ByteString -> Trie a -> Trie a
{-# INLINE delete #-}
delete q = alterBy (\_ _ _ -> Nothing) q (impossible "delete")


{---------------------------------------------------------------
-- Trie-combining functions
---------------------------------------------------------------}

-- | Combine two tries, resolving conflicts by choosing the value
-- from the left trie.
unionL :: Trie a -> Trie a -> Trie a
{-# INLINE unionL #-}
unionL = mergeBy (\x _ -> Just x)

-- | Combine two tries, resolving conflicts by choosing the value
-- from the right trie.
unionR :: Trie a -> Trie a -> Trie a
{-# INLINE unionR #-}
unionR = mergeBy (\_ y -> Just y)

----------------------------------------------------------------
----------------------------------------------------------- fin.
