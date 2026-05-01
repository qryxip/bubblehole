{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bubblehole.RoGh (Request (..), Response (..), isAllowed, runServer, runClient) where

import Bubblehole.Pipe (clientConnect, serverServe)
import Control.Exception (IOException, throwIO, try)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Relude
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Error (isEOFError)
import System.Process (CreateProcess (cwd), createProcess, proc, readCreateProcessWithExitCode, readProcessWithExitCode, waitForProcess)

data Request = Request Text [Text]

instance A.FromJSON Request where
  parseJSON = A.withObject "Request" $ \o -> Request <$> o A..: "cwd" <*> o A..: "args"

instance A.ToJSON Request where
  toJSON (Request c a) = A.object ["cwd" A..= c, "args" A..= a]

data Response = Response Int Text Text

instance A.FromJSON Response where
  parseJSON = A.withObject "Response" $ \o ->
    Response <$> o A..: "statusCode" <*> o A..: "stdout" <*> o A..: "stderr"

instance A.ToJSON Response where
  toJSON (Response c o e) =
    A.object ["statusCode" A..= c, "stdout" A..= o, "stderr" A..= e]

isAllowed :: [Text] -> Bool
isAllowed as =
  "-h"
    `elem` as
    || "--help"
    `elem` as
    || apiCase
    || take 2 as
    == ["repo", "view"]
    || take 2 as
    == ["pr", "list"]
    || take 2 as
    == ["pr", "view"]
    || take 2 as
    == ["pr", "diff"]
    || take 2 as
    == ["issue", "list"]
    || take 2 as
    == ["issue", "view"]
    || take 2 as
    == ["run", "view"]
  where
    apiCase = case as of
      ("api" : rest) -> hasMethodGet rest || not (any isMethodFlag rest)
      _ -> False
    isMethodFlag a = a == "-X" || a == "--method"
    hasMethodGet (a : b : xs) = (isMethodFlag a && b == "GET") || hasMethodGet (b : xs)
    hasMethodGet _ = False

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

intToExitCode :: Int -> ExitCode
intToExitCode 0 = ExitSuccess
intToExitCode n = ExitFailure n

runServer :: IO ()
runServer = do
  TIO.hPutStrLn stderr ("bubblehole server: listening" :: Text)
  serverServe $ \pipe -> do
    r <- try @SomeException (handleOne pipe)
    case r of
      Left e -> TIO.hPutStrLn stderr (T.pack ("server error: " <> show e))
      Right () -> pass

handleOne :: (Handle, Handle) -> IO ()
handleOne (hI, hO) = do
  result <- try @IOException (BS.hGetLine hI)
  case result of
    Left ex | isEOFError ex -> pass
    Left ex -> throwIO ex
    Right reqLine -> do
      resp <- case A.eitherDecodeStrict reqLine of
        Left e -> pure $ Response 1 "" (T.pack ("invalid request: " <> e))
        Right req -> processRequest req
      BSL.hPut hO (A.encode resp)
      BSL.hPut hO "\n"
      hFlush hO

processRequest :: Request -> IO Response
processRequest (Request c as) =
  if isAllowed as
    then do
      let p = (proc "gh" (map T.unpack as)) {cwd = Just (T.unpack c)}
      (code, out, err) <- readCreateProcessWithExitCode p ""
      pure $ Response (exitCodeToInt code) (T.pack out) (T.pack err)
    else pure $ Response 1 "" "please run outside the Claude sandbox"

runClient :: [String] -> IO ()
runClient as = do
  authResult <- try @SomeException (readProcessWithExitCode "gh" ["auth", "status"] "")
  case authResult of
    Right (ExitSuccess, _, _) -> runDirect as
    _ -> runViaIpc as

runDirect :: [String] -> IO ()
runDirect as = do
  (_, _, _, ph) <- createProcess (proc "gh" as)
  code <- waitForProcess ph
  exitWith code

runViaIpc :: [String] -> IO ()
runViaIpc as = do
  cwdPath <- getCurrentDirectory
  (hI, hO) <- clientConnect
  let r = Request (T.pack cwdPath) (map T.pack as)
  BSL.hPut hO (A.encode r)
  BSL.hPut hO "\n"
  hFlush hO
  respLine <- BS.hGetLine hI
  case A.eitherDecodeStrict respLine of
    Left e -> do
      TIO.hPutStrLn stderr (T.pack ("invalid response: " <> e))
      exitWith (ExitFailure 1)
    Right (Response code out err) -> do
      TIO.putStr out
      TIO.hPutStr stderr err
      exitWith (intToExitCode code)
