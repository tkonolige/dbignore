{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

{-
TODO: there might be so weird conditions where the cache does not get updated properly
-}

module Ignore where

import Foreign.C.Types
import Foreign.C.String

import System.Directory
import System.Posix.FilePath

import Data.ByteString as B
import Data.ByteString.Char8 as B8 (pack, unpack, split)

import Data.Trie as T

import Control.Monad

import Control.Concurrent.MVar
import System.IO.Unsafe

-- global cache of dbignore files
cacheVar :: MVar (Trie [ByteString])
{-# NOINLINE cacheVar #-}
cacheVar = unsafePerformIO (newMVar T.empty)

dbignoreName :: ByteString
dbignoreName = ".dbignore"

dropboxPath :: RawFilePath
dropboxPath = "/Users/tkonolige/Dropbox"

isDBIgnore :: RawFilePath -> Bool
isDBIgnore = isSuffixOf dbignoreName

ignore :: RawFilePath -> IO Bool
ignore file = case isPrefixOf dropboxPath file of -- coarse filter
  True -> do
    modifyMVar cacheVar $ \cache -> do
    B.putStrLn file
    res <- case isDBIgnore file of
             True  -> do
               regexs <- B.readFile $ B8.unpack file
               let splits = B8.split '\n' regexs
               return (insert (takeDirectory file) splits cache, False)
             False -> case nearestMatch file cache of
                        Just (path, regexs) -> go regexs >>= return . (,) cache
                         where
                           go :: [ByteString] -> IO Bool
                           go (r:rs) = do
                             f <- fnmatch (path `append` "/" `append` r) file -- regexs might be located at a higher directory
                             case f of 
                               True  -> return True
                               False -> go rs
                           go [] = return False
                        Nothing -> return (cache, False) -- could not find any ignore files
    print $ snd res
    return res
  False -> return False

boolToCInt :: Bool -> CInt
boolToCInt b = case b of
                 True  -> 1
                 False -> 0

ignore_hs :: CString -> IO CInt
ignore_hs str = packCString str >>= (liftM boolToCInt) . ignore

foreign export ccall ignore_hs :: CString -> IO CInt

foreign import ccall "fnmatch.h fnmatch" c_fnmatch :: CString -> CString -> CInt -> CInt

fnmatch :: ByteString -> ByteString -> IO Bool
fnmatch patb strb = useAsCString patb $ \pat -> useAsCString strb $ \str -> do
  case c_fnmatch pat str 0 of
    0 -> return True
    _ -> return False

-- add_to_trie_hs :: IO ()
-- add_to_trie_hs = do
--   B.putStrLn "_______________________"
--
-- foreign export ccall add_to_trie_hs :: IO ()
