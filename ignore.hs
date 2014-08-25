-- Copyright   :  (C) 2014 Tristan Konolige
-- License     :  BSD-style (see the file LICENSE)

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Maybe

import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Exception.Base

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

data Config = Config { dbPath :: RawFilePath
                     , dbName :: ByteString
                     , debug  :: Bool
                     }
                     deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         return B.empty <*> -- dbPath unknown at this point
                         (fmap B8.pack $ v .:? "ignore_file_name" .!= ".dbignore") <*>
                         v .:? "debug" .!= False
  parseJSON _          = mzero

defaultConfig = Config { dbPath = B.empty, dbName = ".dbignore", debug = False }

config :: MVar Config
{-# NOINLINE config #-}
config = unsafePerformIO (newMVar defaultConfig)

debugMsg :: Config -> String -> IO ()
debugMsg conf msg = handle (\(e :: IOException) -> return ()) $ do -- TODO: write concurrently
  if debug conf then do
    Prelude.appendFile (B8.unpack (dbPath conf) ++ "/../dbignore_log") (msg ++ "\n")
  else
    return ()

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

isDBIgnore :: Config -> RawFilePath -> Bool
isDBIgnore conf = isSuffixOf (dbName conf)

-- add patterns in an ignore file to the cache
addIgnore :: RawFilePath -> Trie [Pattern] -> IO (Trie [Pattern])
addIgnore file trie = handle (\(e :: IOException) -> return trie) $ do
  regexs <- B.readFile $ B8.unpack file
  let splits = Prelude.map (compile . B8.unpack . (takeDirectory file `append` "/" `append`)) $ B8.lines regexs
  return $ T.insert (takeDirectory file) splits trie

readConfig :: RawFilePath -> IO Config
readConfig dbpath = handle (\(e :: IOException) -> return defaultConfig) $ do
  conf <- B.readFile $ B8.unpack $ dbpath `append` "/.dbignore_config"
  let cc = fromMaybe defaultConfig $ decodeStrict' conf
  return $ cc {dbPath = dbpath};

-- initialize the cache
initialize :: IO ()
initialize = do
  dbpath <- getDropboxPath
  conf <- readConfig dbpath
  debugMsg conf "Initializing"
  modifyMVar_ config $ const $ return conf
  ignores <- globDir1 (compile $ "**/" ++ B8.unpack (dbName conf)) $ B8.unpack dbpath
  debugMsg conf $ "Found ignore files:"
  mapM_ (debugMsg conf . (++) "  ") ignores
  modifyMVar_ cacheVar $ \cache ->
    foldM (flip addIgnore) cache $ Prelude.map B8.pack ignores

-- the main ignore function
-- determines if a filepath should be ignored or not
ignore :: RawFilePath -> IO Bool
ignore file = do
  modifyMVar_ initialized $ \case
                              True  -> return True
                              False -> initialize >> return True
  conf <- readMVar config
  case isPrefixOf (dbPath conf) file of -- coarse filter
    True -> do
      debugMsg conf $ "Ignore on " ++ B8.unpack file
      modifyMVar cacheVar $ \cache -> do
        res <- case isDBIgnore conf file of -- check to see if this is .dbignore
                 True  -> do
                   debugMsg conf "  is ignore file"
                   t <- addIgnore file cache
                   return (t, False)
                 False -> do
                   debugMsg conf " is not ignore file"
                   case nearestMatch file cache of -- find the nearest .dbignore
                     Just (path, regexs) -> do
                       debugMsg conf $ "  nearest ignore file is " ++ B8.unpack path
                       doesMatch regexs >>= return . (,) cache
                      where
                        doesMatch :: [Pattern] -> IO Bool -- TODO: looks like a fold
                        doesMatch (r:rs) = do
                          case match r (B8.unpack file) of 
                            True  -> do
                              debugMsg conf $ "    " ++ decompile r ++ " matches!"
                              return True
                            False -> do
                              debugMsg conf $ "    " ++ decompile r ++ " does not match"
                              doesMatch rs
                        doesMatch [] = return False
                     Nothing -> do
                       debugMsg conf "  no ignore file found"
                       return (cache, False) -- could not find any ignore files
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
