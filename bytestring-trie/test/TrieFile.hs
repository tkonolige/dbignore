{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.02.12
-- |
-- Module      :  Bench.TrieFile
-- Copyright   :  Copyright (c) 2008--2009 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Example program to read each line from a file (e.g. @/usr/dict@)
-- into a trie. Used to benchmark the naive algorithm against the
-- optimized C. Also used to verify correctness of C implementation,
-- and Trie implementation in general.
----------------------------------------------------------------

module Main (main, readTrieFromFile) where

import qualified Data.Trie             as T
import qualified Data.Trie.Internal    as T
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S (unlines)

import qualified Data.Foldable         as F

{-
import Microbench
import Control.Exception (evaluate)
-- -}

import System.IO          (withFile, IOMode(..), hIsEOF)
import System.Environment (getArgs)
----------------------------------------------------------------

-- Broken out for manual use in GHCi
readTrieFromFile :: FilePath -> IO (T.Trie Int)
readTrieFromFile file = withFile file ReadMode $ \h ->
    let go i t = do { b <- hIsEOF h
                    ; if b
                      then return t
                      else do { line <- S.hGetLine h
                              ; (go $! i+1) $! T.insert line i t
                              }
                    }
    in go 0 T.empty

main :: IO ()
main  = do
    [file] <- getArgs
    t <- readTrieFromFile file -- >>= evaluate
    
    -- `sort`
    -- S.putStrLn . S.unlines . T.keys $ t
    
    -- `wc -l`
    -- putStrLn . show . T.size $ t
    
    
    {- -- Tests for comparing inferred foldl/foldr vs hand-written version
    microbench "List.foldr elems" $ do
        vs <- return $! T.elems t
        n  <- return $! foldr (\v r -> v `seq` (1+) $! r) (0::Int) vs
        n `seq` (return () :: IO ())
        
    microbench "Trie.foldr @elems" $ do
        t' <- return $! t
        n  <- return $! F.foldr (\v r -> v `seq` (1+) $! r) (0::Int) t'
        n `seq` (return () :: IO ())
        
    microbench "Trie.foldl @elems" $ do
        t' <- return $! t
        n  <- return $! F.foldl (\r v -> v `seq` (1+) $! r) (0::Int) t'
        n `seq` (return () :: IO ())
    -- -}
    
    -- {- -- verify associativity of folds
    putStrLn . show . take 20 . F.foldr (:) [] $ t
    putStrLn . show . take 20 . T.foldrWithKey (const (:)) [] $ t
    
    putStrLn . show . take 20 . F.foldl (flip (:)) [] $ t
    -- -}


----------------------------------------------------------------
----------------------------------------------------------- fin.
