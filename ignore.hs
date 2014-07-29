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
import System.FilePath.Glob

import Data.ByteString as B
import Data.ByteString.Char8 as B8 (pack, unpack, split)
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

addIgnore :: RawFilePath -> Trie [Pattern] -> IO (Trie [Pattern])
addIgnore file trie = do
  regexs <- B.readFile $ B8.unpack file
  let splits = Prelude.map (compile . B8.unpack . (takeDirectory file `append` "/" `append`)) $ B8.split '\n' regexs
  return $ T.insert (takeDirectory file) splits trie

ignore :: RawFilePath -> IO Bool
ignore file = do
  modifyMVar_ initialized $ \init -> case init of
                                        True  -> return True
                                        False -> do
                                          dbpath <- getDropboxPath
                                          modifyMVar_ dropboxPath $ \_ -> return dbpath
                                          ignores <- globDir1 (compile "**/.dbignore") $ B8.unpack dbpath
                                          modifyMVar_ cacheVar $ \cache -> foldM (flip addIgnore) cache $ Prelude.map B8.pack ignores
                                          return True
  dbpath <- readMVar dropboxPath
  case isPrefixOf dbpath file of -- coarse filter
    True -> do
      modifyMVar cacheVar $ \cache -> do
        -- B.putStrLn file
        res <- case isDBIgnore file of
                 True  -> do
                   t <- addIgnore file cache
                   return (t, False)
                 False -> case nearestMatch file cache of
                            Just (path, regexs) -> go regexs >>= return . (,) cache
                             where
                               go :: [Pattern] -> IO Bool
                               go (r:rs) = do
                                 case match r (B8.unpack file) of 
                                   True  -> return True
                                   False -> go rs
                               go [] = return False
                            Nothing -> return (cache, False) -- could not find any ignore files
        -- print $ snd res
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
