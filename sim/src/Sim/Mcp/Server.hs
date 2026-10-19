{-# LANGUAGE OverloadedStrings #-}

-- MCP (Model Context Protocol) server over stdio: newline-delimited
-- JSON-RPC 2.0. Tool calls run on forkIO threads so a slow tool never
-- blocks the read loop; the client cancels a call with the
-- notifications/cancelled notification (matching requestId), which sets
-- the call's cancel flag — long-running tools poll it (SimHooks,
-- TrainHooks, OptHooks all wire to it) and long runners report through
-- the notifications/progress notification when the call carried a
-- progressToken.
module Sim.Mcp.Server
  ( Tool (..)
  , ToolCtx (..)
  , serve
  , toolTextResult
  , toolErrorResult
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitSuccess)
import System.IO (Handle, hFlush, hIsEOF, stdin, stdout)

-- one tool: schema is a JSON Schema object for tools/list; run receives
-- the call's arguments object plus a context with progress reporting
-- and a cancellation poll
data Tool = Tool
  { toolName        :: !Text
  , toolDescription :: !Text
  , toolSchema      :: !Value
  , toolRun         :: ToolCtx -> Value -> IO Value
  }

data ToolCtx = ToolCtx
  { ctxProgress  :: !(Double -> IO ())
    -- ^ emit a notifications/progress for this call (no-op without a token)
  , ctxCancelled :: !(IO Bool)
    -- ^ polls True once the client cancels this call
  }

data Server = Server
  { srvTools      :: ![Tool]
  , srvOutLock    :: !(MVar ())
  , srvCancels    :: !(IORef (Map.Map Value (IORef Bool)))
  , srvExitAfter  :: !(IORef Bool)
  }

-- serve until EOF on stdin or an exit notification after shutdown
serve :: [Tool] -> IO ()
serve tools = do
  outLock <- newMVar ()
  cancels <- newIORef Map.empty
  exiting <- newIORef False
  let server = Server tools outLock cancels exiting
  loop server
  where
    loop server = do
      eof <- hIsEOF stdin
      if eof
        then pure ()
        else do
          line <- B8.getLine
          case decode (BL8.fromStrict line) of
            Nothing  -> loop server
            Just msg -> do
              stop <- dispatch server msg
              if stop then pure () else loop server

-- returns True when the server should shut down
dispatch :: Server -> Value -> IO Bool
dispatch server msg = case msg of
  Object o -> do
    case KM.lookup "method" o of
      Nothing -> pure False -- a client response: we never send requests, ignore
      Just (String method) -> do
        let params = maybe (Object KM.empty) id (KM.lookup "params" o)
            reqId = KM.lookup "id" o
        case reqId of
          Just rid -> handleRequest server method rid params
          Nothing  -> handleNotification server method params
      Just _ -> pure False
  _ -> pure False

handleRequest :: Server -> Text -> Value -> Value -> IO Bool
handleRequest server method reqId params = case method of
  "initialize" -> do
    reply server reqId $ object
      [ "protocolVersion" .= ("2025-06-18" :: Text)
      , "capabilities" .= object ["tools" .= object ["listChanged" .= False]]
      , "serverInfo" .= object ["name" .= ("rel-sim" :: Text), "version" .= ("0.1" :: Text)]
      ]
    pure False
  "ping" -> do
    reply server reqId (object [])
    pure False
  "tools/list" -> do
    reply server reqId $ object ["tools" .= map toolJson (srvTools server)]
    pure False
  "tools/call" -> do
    void $ forkIO (runToolCall server reqId params)
    pure False
  "shutdown" -> do
    reply server reqId Null
    writeIORef (srvExitAfter server) True
    pure False
  _ -> do
    replyErr server reqId (-32601) ("method not found: " ++ T.unpack method)
    pure False

handleNotification :: Server -> Text -> Value -> IO Bool
handleNotification server method params = case method of
  "notifications/cancelled" -> do
    case params of
      Object o | Just rid <- KM.lookup "requestId" o -> cancelCall server rid
      _ -> pure ()
    pure False
  "exit" -> do
    ex <- readIORef (srvExitAfter server)
    if ex then exitSuccess else pure False
  _ -> pure False -- initialized, setLevel, progress (client->server): ignore

cancelCall :: Server -> Value -> IO ()
cancelCall server rid = do
  m <- readIORef (srvCancels server)
  case Map.lookup rid m of
    Just ref -> writeIORef ref True
    Nothing  -> pure ()

runToolCall :: Server -> Value -> Value -> IO ()
runToolCall server reqId params = do
  let name = case params of
        Object o -> case KM.lookup "name" o of
          Just (String n) -> n
          _               -> ""
        _ -> ""
      args = case params of
        Object o -> maybe (Object KM.empty) id (KM.lookup "arguments" o)
        _        -> Object KM.empty
      token = case params of
        Object o -> case KM.lookup "_meta" o of
          Just (Object m) -> KM.lookup "progressToken" m
          _               -> KM.lookup "progressToken" o
        _ -> Nothing
  case filter (\t -> toolName t == name) (srvTools server) of
    []    -> replyErr server reqId (-32602) ("unknown tool: " ++ T.unpack name)
    (t:_) -> do
      cancelled <- newIORef False
      atomicModifyIORef' (srvCancels server) (\m -> (Map.insert reqId cancelled m, ()))
      let emitProgress = case token of
            Just tok -> \frac ->
              send server $ object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("notifications/progress" :: Text)
                , "params" .= object
                    [ "progressToken" .= tok
                    , "progress" .= frac
                    , "total" .= (1 :: Double)
                    ]
                ]
            Nothing -> \_ -> pure ()
          ctx = ToolCtx emitProgress (readIORef cancelled)
      result <- (Right <$> toolRun t ctx args) `catch` \e ->
        pure (Left (show (e :: SomeException)))
      atomicModifyIORef' (srvCancels server) (\m -> (Map.delete reqId m, ()))
      case result of
        Left err -> replyErr server reqId (-32000) err
        Right v  -> reply server reqId v

-- a standard tool result wrapping a JSON payload as text content
toolTextResult :: Value -> Value
toolTextResult payload = object
  [ "content" .= [object ["type" .= ("text" :: Text), "text" .= encodeText payload]]
  ]

toolErrorResult :: String -> Value
toolErrorResult msg = object
  [ "content" .= [object ["type" .= ("text" :: Text), "text" .= T.pack msg]]
  , "isError" .= True
  ]

toolJson :: Tool -> Value
toolJson t = object
  [ "name" .= toolName t
  , "description" .= toolDescription t
  , "inputSchema" .= toolSchema t
  ]

reply :: Server -> Value -> Value -> IO ()
reply server reqId result = send server $ object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= reqId
  , "result" .= result
  ]

replyErr :: Server -> Value -> Int -> String -> IO ()
replyErr server reqId code msg = send server $ object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= reqId
  , "error" .= object ["code" .= code, "message" .= msg]
  ]

send :: Server -> Value -> IO ()
send server v = modifyMVar_ (srvOutLock server) $ \() -> do
  BL8.putStr (encode v)
  B8.putStr "\n"
  hFlush stdout

-- JSON payload rendered as text for the content field
encodeText :: Value -> Text
encodeText = T.pack . BL8.unpack . encode
