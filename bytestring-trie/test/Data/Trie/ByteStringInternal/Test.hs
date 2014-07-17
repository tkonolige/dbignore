{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.02.06
-- |
-- Module      :  Data.Trie.ByteStringInternal.Test
-- Copyright   :  Copyright (c) 2008--2009 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Testing helper functions on 'ByteString's.
----------------------------------------------------------------


module Data.Trie.ByteStringInternal.Test where
import Data.Trie.ByteStringInternal
import Data.Trie.Test (packC2W)

import Data.List (unfoldr)
----------------------------------------------------------------

-- | For debugging. [] is the infinite bit, head is the little end
showBits :: (Integral a) => a -> String
showBits = unfoldr getBit
    where
    getBit 0             = Nothing
    getBit i | odd i     = Just ('I', (i-1)`div`2)
             | otherwise = Just ('O', i`div`2)


-- TODO: make this into an HUnit test
test :: IO ()
test  = do
    cmp hello
    cmp $ packC2W "hi"
    cmp $ packC2W "heat"
    cmp $ packC2W "held"
    cmp $ packC2W "hell"
    cmp $ packC2W "hello"
    cmp $ packC2W "jello"
    where
    cmp y = do putStrLn . show . breakMaximalPrefix hello $ y
               putStrLn . show . (\(a,b,c) -> (a,c,b)) . breakMaximalPrefix y $ hello
               putStrLn "\n"
    hello = packC2W "hello"
----------------------------------------------------------------
----------------------------------------------------------- fin.