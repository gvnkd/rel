{-# LANGUAGE OverloadedStrings #-}

-- editor.* MCP tools: proxy JSON-line requests to a live Redot editor's
-- chart scene over localhost TCP (chart.gd's MCP bridge, port from the
-- user://mcp_port file). Read-only ops fail cleanly when no editor is
-- running; mutating ops (add/remove timeline, follow) report the same
-- way.
module Sim.Mcp.Editor (editorTools) where

import Control.Exception (IOException, SomeException, catch, try)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (Family (AF_INET), Socket, SocketType (Stream), close, connect, defaultProtocol, socket, tupleToHostAddress)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString.Lazy as NSBL
import Sim.Mcp.Server (Tool (..), ToolCtx, toolErrorResult, toolTextResult)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.IO.Error (ioError, isDoesNotExistError, userError)

editorTools :: [Tool]
editorTools =
  [ toolEditor "editor.status" "Live-editor status: series count, playback head, follow flag."
      "status" []
  , toolEditor "editor.list_series" "List the chart's loaded series (name, size, timeframe, store key, color)."
      "list_series" []
  , toolEditor "editor.add_timeline" "Load a candles csv/rbin into the live chart as a new timeline (or reload a same-name one)."
      "add_timeline" ["path"]
  , toolEditor "editor.remove_series" "Remove a series from the live chart by name or index."
      "remove_series" []
  , toolEditor "editor.get_view" "Read the live chart view: zoom levels and visible index/price ranges."
      "get_view" []
  , toolEditor "editor.set_follow" "Toggle the live chart's follow-tail mode."
      "set_follow" []
  , toolEditor "editor.pause" "Pause or resume the live chart's playback."
      "pause" []
  ]

toolEditor :: Text -> Text -> Text -> [Text] -> Tool
toolEditor name desc op required = Tool
  { toolName = name
  , toolDescription = desc
  , toolSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      , "required" .= required
      ]
  , toolRun = \_ args -> do
      r <- try (callEditor op args) :: IO (Either SomeException Value)
      pure (case r of
        Left err -> toolErrorResult (show err)
        Right v  -> toolTextResult v)
  }

-- send {"id", "op", "args"} to the editor, wait for the matching reply
callEditor :: Text -> Value -> IO Value
callEditor op args = do
  port <- readPort
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (addr port)
  NSBL.sendAll sock (encode (object ["id" .= (1 :: Int), "op" .= op, "args" .= args]) <> "\n")
  reply <- recvLine sock BL8.empty
  close sock
  case decode reply of
    Just (Object o)
      | KM.lookup "ok" o == Just (Bool True), Just res <- KM.lookup "result" o ->
          pure res
      | Just res <- KM.lookup "result" o ->
          pure (object ["_mcp_error" .= res])
    _ -> pure (object ["_mcp_error" .= ("bad reply frame" :: Text)])
  where
    addr p = NS.SockAddrInet (fromIntegral p) (tupleToHostAddress (127, 0, 0, 1))

recvLine :: Socket -> BL8.ByteString -> IO BL8.ByteString
recvLine sock acc = do
  chunk <- NSBL.recv sock 4096
  if BL8.null chunk
    then pure acc
    else case BL8.break (== '\n') (acc <> chunk) of
      (line, rest)
        | BL8.null rest -> recvLine sock (acc <> chunk)
        | otherwise     -> pure line

-- the port file the editor writes (user://mcp_port under the Redot user
-- dir); overridable with the REL_MCP_PORT_FILE env var
readPort :: IO Int
readPort = do
  envPath <- lookupEnv "REL_MCP_PORT_FILE"
  path <- case envPath of
    Just p  -> pure p
    Nothing -> do
      home <- getHomeDirectory
      pure (home ++ "/.local/share/redot/app_userdata/rel/mcp_port")
  exists <- doesFileExist path `catch` \e ->
    if isDoesNotExistError e then pure False else ioError e
  if not exists
    then ioError (userError ("no live editor (missing " ++ path ++ ")"))
    else do
      txt <- readFile path
      case reads (takeWhile (/= '\n') txt) of
        [(p, "")] -> pure p
        _         -> ioError (userError ("bad port file: " ++ path))
