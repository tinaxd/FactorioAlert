{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, catch, handle)
import Control.Exception.Base (Exception)
import Control.Monad (forever)
import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.IO.Handle (Handle, hGetLine)
import Network.HTTP.Req (HttpException, POST (POST), ReqBodyJson (ReqBodyJson), defaultHttpConfig, ignoreResponse, req, runReq, useHttpsURI)
import System.Console.CmdArgs
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import System.Process
import Text.Regex.TDFA
import Text.URI (URI, mkURI)

data Args = Args
  { argFactorioPath :: String,
    argFactorioArgs :: String,
    argDiscordEndpoint :: String,
    argGwEndpoint :: String
  }
  deriving (Show, Data, Typeable)

_args :: Args
_args =
  Args
    { argFactorioPath = def &= name "factorio-path",
      argFactorioArgs = def &= name "factorio-args",
      argDiscordEndpoint = def &= name "discord-webhook",
      argGwEndpoint = def &= name "gw-endpoint"
    }

data Config = Config
  { factorioPath :: String,
    factorioArgs :: [String],
    discordEndpoint :: URI,
    gwEndpoint :: URI
  }
  deriving (Show)

-- data FactorioCallback = FactorioCallback {
--     cbOnJoin :: String -> IO (),
--     cbOnLeave :: String -> IO ()
-- }

data FactorioLine = FactorioJoin String | FactorioLeave String deriving (Show)

watchProcessSignalHandler :: String -> IO ()
watchProcessSignalHandler sig = do
  putStrLn $ "[" ++ sig ++ "] watcher process ignores signals"

readArgs :: Args -> Maybe Config
readArgs arg = do
  disco <- mkURI . T.pack $ argDiscordEndpoint arg
  gw <- mkURI . T.pack $ argGwEndpoint arg
  let fArgs = words $ argFactorioArgs arg
  return $
    Config
      { factorioPath = argFactorioPath arg,
        factorioArgs = fArgs,
        discordEndpoint = disco,
        gwEndpoint = gw
      }

main :: IO ()
main = do
  cmd <- cmdArgs _args
  let config_ = readArgs cmd
  case config_ of
    Nothing -> putStrLn "Invalid arguments"
    Just config -> do
      _ <- installHandler sigTERM (Catch (watchProcessSignalHandler "SIGTERM")) Nothing
      _ <- installHandler sigINT (Catch (watchProcessSignalHandler "SIGINT")) Nothing
      watchFactorio config

createFactorioProcess :: String -> [String] -> IO (Handle, ProcessHandle)
createFactorioProcess path args_ = do
  (_, Just hout, _, ph) <- createProcess process
  return (hout, ph)
  where
    process =
      (proc path args_)
        { std_out = CreatePipe
        }

detectEvent :: String -> Maybe FactorioLine
detectEvent line = detect joinMatches leaveMatches
  where
    joinRe = "\\[JOIN\\] (.*) joined the game$" :: String
    leaveRe = "\\[LEAVE\\] (.*) left the game$" :: String
    (_, _, _, joinMatches) = line =~ joinRe :: (String, String, String, [String])
    (_, _, _, leaveMatches) = line =~ leaveRe :: (String, String, String, [String])
    detect [join] _ = Just $ FactorioJoin join
    detect _ [leave] = Just $ FactorioLeave leave
    detect _ _ = Nothing

watchFactorio :: Config -> IO ()
watchFactorio config = do
  (hout, ph) <- createFactorioProcess (factorioPath config) (factorioArgs config)
  catch
    ( forever $ do
        line <- hGetLine hout
        putStrLn line -- echo back
        case detectEvent line of
          Nothing -> return ()
          Just (FactorioJoin playerName) -> do
            sendDiscordNotification disco True playerName
            sendGwCheck gw True playerName
          Just (FactorioLeave playerName) -> do
            sendDiscordNotification disco False playerName
            sendGwCheck gw False playerName
    )
    ( \e -> do
        putStrLn $ "Factorio process: " ++ show (e :: IOException)
        terminateProcess ph
        exitCode <- waitForProcess ph
        putStrLn $ "Factorio process has been exited with code: " ++ show exitCode
    )
  where
    disco = discordEndpoint config
    gw = gwEndpoint config

sendDiscordNotification :: URI -> Bool -> String -> IO ()
sendDiscordNotification endpoint isJoin playerName = do
  sendDiscordWebhook endpoint content
  where
    content =
      if isJoin then playerName ++ " が Factorio サーバーに参加しました" else playerName ++ " が Factorio サーバーから退出しました"

sendDiscordWebhook :: URI -> String -> IO ()
sendDiscordWebhook endpoint content = do
  let url_ = useHttpsURI endpoint
  case url_ of
    Nothing -> putStrLn "Invalid endpoint"
    Just (url, _) -> do
      handle (showAndIgnoreError :: HttpException -> IO ()) $ do
        _res <- runReq defaultHttpConfig $ req POST url (ReqBodyJson contentJson) ignoreResponse mempty
        putStrLn $ "Discord webhook: " ++ show _res
        return ()
  where
    contentJson =
      object
        [ "content" .= content,
          "username" .= ("FactorioAlert" :: String)
        ]

sendGwCheck :: URI -> Bool -> String -> IO ()
sendGwCheck endpoint isJoin playerName = do
  let url_ = useHttpsURI endpoint
  currentTime <- iso8601Show <$> getCurrentTime
  case url_ of
    Nothing -> putStrLn "Invalid endpoint"
    Just (url, _) -> do
      handle (showAndIgnoreError :: HttpException -> IO ()) $ do
        _res <- runReq defaultHttpConfig $ req POST url (ReqBodyJson (contentJson currentTime)) ignoreResponse mempty
        putStrLn $ "Gamewatch check: " ++ show _res
        return ()
  where
    contentJson currentTime =
      object
        [ "in_game_name" .= playerName,
          "type" .= joinStr,
          "time" .= currentTime,
          "game_name" .= ("Factorio" :: String)
        ]
    joinStr :: String
    joinStr = if isJoin then "start" else "stop"

showAndIgnoreError :: (Exception e) => e -> IO ()
showAndIgnoreError e = putStrLn $ "Error: " ++ show e