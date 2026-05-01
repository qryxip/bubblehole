{-# LANGUAGE NoImplicitPrelude #-}

module Bubblehole.Pipe.Unix (serverServe, clientConnect) where

import Control.Exception (finally)
import Relude
import System.IO (hClose)
import System.Posix.Files (createNamedPipe, fileExist, ownerReadMode, ownerWriteMode, removeLink, unionFileModes)
import System.Posix.IO (OpenMode (ReadOnly, WriteOnly), defaultFileFlags, fdToHandle, openFd)

inPath, outPath :: FilePath
inPath = "/tmp/bubblehole.in"
outPath = "/tmp/bubblehole.out"

openFifo :: FilePath -> OpenMode -> IO Handle
openFifo path mode = openFd path mode defaultFileFlags >>= fdToHandle

serverServe :: ((Handle, Handle) -> IO ()) -> IO ()
serverServe handler = do
  mapM_ (\p -> whenM (fileExist p) (removeLink p)) [inPath, outPath]
  let mode = unionFileModes ownerReadMode ownerWriteMode
  createNamedPipe inPath mode
  createNamedPipe outPath mode
  forever $ do
    hIn <- openFifo inPath ReadOnly
    hOut <- openFifo outPath WriteOnly
    handler (hIn, hOut) `finally` (hClose hIn >> hClose hOut)

clientConnect :: IO (Handle, Handle)
clientConnect = do
  hOut <- openFifo inPath WriteOnly
  hIn <- openFifo outPath ReadOnly
  pure (hIn, hOut)
