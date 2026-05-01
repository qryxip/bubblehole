{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Bubblehole.RoGh (runClient, runServer)
import Options.Applicative (ParserInfo, command, execParser, forwardOptions, fullDesc, helper, info, metavar, progDesc, strArgument, subparser)
import Relude

data Opts = Server | Client [String]

opts :: ParserInfo Opts
opts = info (subparser cmds <**> helper) (fullDesc <> progDesc desc)
  where
    desc = "Read-only gh proxy over a named pipe"
    cmds =
      command "server" (info serverCmd (progDesc serverDesc))
        <> command "client" (info clientCmd (progDesc clientDesc <> forwardOptions))
    serverCmd = pure Server
    serverDesc = "Run the proxy daemon"
    clientCmd = Client <$> many (strArgument (metavar "GH_ARGS..."))
    clientDesc = "Forward args to gh through the daemon"

main :: IO ()
main =
  execParser opts >>= \case
    Server -> runServer
    Client args -> runClient args
