-- Copyright   :  (C) 2014 Tristan Konolige
-- License     :  BSD-style (see the file LICENSE)

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-
TODO: there might be so weird conditions where the cache does not get updated properly
-}

module Ignore where

import Foreign.C.Types
import Foreign.C.String

import System.Directory
import System.Posix.FilePath
import System.FilePath.Glob

import Data.ByteString as B
import Data.ByteString.Char8 as B8 (pack, unpack, split, lines)
import qualified Data.Text as Text

import Data.Trie as T

import Control.Monad

import Control.Concurrent.MVar
import System.IO.Unsafe

import Data.Aeson
import Data.HashMap.Strict


-- global cache of dbignore files
cacheVar :: MVar (Trie [Pattern])
{-# NOINLINE cacheVar #-}
cacheVar = unsafePerformIO (newMVar T.empty)

initialized :: MVar Bool
{-# NOINLINE initialized #-}
initialized = unsafePerformIO (newMVar False)

dbignoreName :: ByteString
dbignoreName = ".dbignore"

dropboxPath :: MVar RawFilePath
{-# NOINLINE dropboxPath #-}
dropboxPath = unsafePerformIO (newMVar B.empty)

-- figure out dropbox directory
-- Dropbox stores its location inside ~/.dropbox/info.json
getDropboxPath :: IO RawFilePath
getDropboxPath = do
  home <- getHomeDirectory
  let infoFile = home ++ "/.dropbox/info.json"
  info <- B.readFile infoFile
  let Just (Object o) = decodeStrict' info
      Object pp = o ! "personal"
      String path = pp ! "path"
  return $ B8.pack $ Text.unpack path

isDBIgnore :: RawFilePath -> Bool
isDBIgnore = isSuffixOf dbignoreName

-- add patterns in an ignore file to the cache
addIgnore :: RawFilePath -> Trie [Pattern] -> IO (Trie [Pattern])
addIgnore file trie = do
  regexs <- B.readFile $ B8.unpack file
  let splits = Prelude.map (compile . B8.unpack . (takeDirectory file `append` "/" `append`)) $ B8.lines regexs
  return $ T.insert (takeDirectory file) splits trie

-- initialize the cache
initialize :: IO ()
initialize = do 
  dbpath <- getDropboxPath
  modifyMVar_ dropboxPath $ \_ -> return dbpath
  ignores <- globDir1 (compile $ "**/" ++ B8.unpack dbignoreName) $ B8.unpack dbpath
  modifyMVar_ cacheVar $ \cache ->
    foldM (flip addIgnore) cache $ Prelude.map B8.pack ignores

-- the main ignore function
-- determines if a filepath should be ignored or not
ignore :: RawFilePath -> IO Bool
ignore file = do
  modifyMVar_ initialized $ \case
                              True  -> return True
                              False -> initialize >> return True
  dbpath <- readMVar dropboxPath
  case isPrefixOf dbpath file of -- coarse filter
    True -> do
      modifyMVar cacheVar $ \cache -> do
        res <- case isDBIgnore file of -- check to see if this is .dbignore
                 True  -> do
                   t <- addIgnore file cache
                   return (t, False)
                 False -> case nearestMatch file cache of -- find the nearest .dbignore
                            Just (path, regexs) -> doesMatch regexs >>= return . (,) cache
                             where
                               doesMatch :: [Pattern] -> IO Bool -- TODO: looks like a fold
                               doesMatch (r:rs) = do
                                 case match r (B8.unpack file) of 
                                   True  -> return True
                                   False -> doesMatch rs
                               doesMatch [] = return False
                            Nothing -> return (cache, False) -- could not find any ignore files
        return res
    False -> return False

boolToCInt :: Bool -> CInt
boolToCInt b = case b of
                 True  -> 1
                 False -> 0

-- c wrapper for ignore
ignore_hs :: CString -> IO CInt
ignore_hs str = packCString str >>= (liftM boolToCInt) . ignore

foreign export ccall ignore_hs :: CString -> IO CInt
