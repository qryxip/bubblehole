{-# LANGUAGE NoImplicitPrelude #-}

module Bubblehole.Pipe.Win32 (serverServe, clientConnect) where

import Control.Exception (finally)
import Data.Bits ((.|.))
import GHC.IO.Device (IODeviceType (Stream))
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
import Relude
import System.IO (IOMode (ReadWriteMode), hClose)
import System.Win32.File (createFile, gENERIC_READ, gENERIC_WRITE, oPEN_EXISTING)
import System.Win32.NamedPipes (connectNamedPipe, createNamedPipe, pIPE_ACCESS_DUPLEX, pIPE_READMODE_BYTE, pIPE_TYPE_BYTE, pIPE_WAIT, waitNamedPipe)

pipeName :: String
pipeName = "\\\\.\\pipe\\bubblehole"

serverServe :: ((Handle, Handle) -> IO ()) -> IO ()
serverServe handler = forever $ do
  h <-
    createNamedPipe
      pipeName
      pIPE_ACCESS_DUPLEX
      (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE .|. pIPE_WAIT)
      1
      4096
      4096
      0
      Nothing
  connectNamedPipe h Nothing
  rh <- mkHandleFromHANDLE h Stream "pipe" ReadWriteMode Nothing
  handler (rh, rh) `finally` hClose rh

clientConnect :: IO (Handle, Handle)
clientConnect = do
  waitNamedPipe pipeName 5000
  h <- createFile pipeName (gENERIC_READ .|. gENERIC_WRITE) 0 Nothing oPEN_EXISTING 0 Nothing
  rh <- mkHandleFromHANDLE h Stream "pipe" ReadWriteMode Nothing
  pure (rh, rh)
