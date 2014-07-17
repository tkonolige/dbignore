{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2011.02.12
-- |
-- Module      :  Data.Trie.Errors
-- Copyright   :  Copyright (c) 2008--2011 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal convenience functions for giving error messages.
----------------------------------------------------------------

module Data.Trie.Errors
    ( impossible
    ) where

----------------------------------------------------------------
----------------------------------------------------------------

-- | The impossible happened. Use this instead of 'undefined' just in case.
impossible :: String -> a
{-# NOINLINE impossible #-}
impossible fn =
    error $ "Data.Trie." ++ fn ++ ": the impossible happened. This is a bug, please report it to the maintainer."

----------------------------------------------------------------
----------------------------------------------------------- fin.
