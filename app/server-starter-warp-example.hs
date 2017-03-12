{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Main where

import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.ServerStarter.Socket as Starter
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp


app :: Wai.Application
app _ respond = do
  putStrLn "Got request"
  respond $ Wai.responseLBS
    HTTPTypes.status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

main :: IO ()
main = do
  (socket:_) <- Starter.listenAll
  print (Socket.fdSocket socket)
  port <- flip fmap (Socket.getSocketName socket) $ \addr -> case addr of
    Socket.SockAddrInet p _ -> p
    Socket.SockAddrUnix _   -> 0
  print port
  let setting = Warp.setHTTP2Disabled
              . Warp.setPort (fromEnum port)
              $ Warp.defaultSettings
  Warp.runSettingsSocket setting socket app
  putStrLn "Finished an old server"
  return ()
