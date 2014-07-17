{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.01.04
-- |
-- Module      :  Bench.FromList
-- Copyright   :  Copyright (c) 2008--2009 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Benchmarking for left- vs right-fold for @fromList@.
----------------------------------------------------------------

module Main where

import qualified Data.Trie as T
import Data.Trie.Convenience (insertIfAbsent)
import Data.List             (foldl', sort)
import qualified Data.ByteString as S
import Data.ByteString.Internal (c2w)

import Microbench
import Control.Exception     (evaluate)
----------------------------------------------------------------

fromListR, fromListL :: [(S.ByteString, a)] -> T.Trie a
fromListR = foldr  (uncurry T.insert) T.empty
fromListL = foldl' (flip $ uncurry $ insertIfAbsent) T.empty

getList, getList'  :: S.ByteString -> Int -> [(S.ByteString, Int)]
getList  xs n = map (\k -> (k,0)) . S.inits . S.take n $ xs
getList' xs n = map (\k -> (k,0)) . S.tails . S.take n $ xs

main :: IO ()
main  = do
    -- 100000 is large enough to trigger Microbench's stop condition,
    -- and small enough to not lock up the system in trying to create it.
    xs <- evaluate $ S.replicate 100000 (c2w 'a')
    
    microbench "fromListR obverse" (T.null . fromListR . getList xs)
    microbench "fromListL obverse" (T.null . fromListL . getList xs)
    
    putStrLn ""
    microbench "fromListR reverse" (T.null . fromListR . getList' xs)
    microbench "fromListL reverse" (T.null . fromListL . getList' xs)
    
    -- Sorting forces it into the obverse order at O(n log n) cost
    putStrLn ""
    putStrLn ""
    microbench "fromListR obverse sorted" (T.null . fromListR . sort . getList xs)
    microbench "fromListL obverse sorted" (T.null . fromListL . sort . getList xs)
    putStrLn ""
    microbench "fromListR reverse sorted" (T.null . fromListR . sort . getList' xs)
    microbench "fromListL reverse sorted" (T.null . fromListL . sort . getList' xs)

----------------------------------------------------------------
----------------------------------------------------------- fin.
