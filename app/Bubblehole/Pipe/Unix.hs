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
openFifo p m = openFd p m defaultFileFlags >>= fdToHandle

serverServe :: ((Handle, Handle) -> IO ()) -> IO ()
serverServe f = do
  mapM_ (\p -> whenM (fileExist p) (removeLink p)) [inPath, outPath]
  let mode = unionFileModes ownerReadMode ownerWriteMode
  createNamedPipe inPath mode
  createNamedPipe outPath mode
  forever $ do
    hI <- openFifo inPath ReadOnly
    hO <- openFifo outPath WriteOnly
    f (hI, hO) `finally` (hClose hI >> hClose hO)

clientConnect :: IO (Handle, Handle)
clientConnect = do
  hO <- openFifo inPath WriteOnly
  hI <- openFifo outPath ReadOnly
  pure (hI, hO)
