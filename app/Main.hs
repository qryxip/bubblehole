{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Bubblehole.RoGh (runClient, runServer)
import Options.Applicative (ParserInfo, command, execParser, forwardOptions, fullDesc, helper, info, metavar, progDesc, strArgument, subparser)
import Relude

data Opts = Server | Client [String]

opts :: ParserInfo Opts
opts = info (subparser cs <**> helper) (fullDesc <> progDesc "Read-only gh proxy over a named pipe")
  where
    cs =
      command "server" (info (pure Server) (progDesc "Run the proxy daemon"))
        <> command "client" (info clientP (progDesc "Forward args to gh through the daemon" <> forwardOptions))
    clientP = Client <$> many (strArgument (metavar "GH_ARGS..."))

main :: IO ()
main =
  execParser opts >>= \case
    Server -> runServer
    Client args -> runClient args
