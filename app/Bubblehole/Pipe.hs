{-# LANGUAGE CPP #-}

module Bubblehole.Pipe (module P) where

#ifdef mingw32_HOST_OS
import Bubblehole.Pipe.Win32 as P
#else
import Bubblehole.Pipe.Unix as P
#endif
