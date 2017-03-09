{-# LANGUAGE Strict #-}
module Network.ServerStarter.Socket
    ( listenAll
    ) where

import Foreign.C.Types (CInt)
import qualified Network.Socket as Socket
import qualified System.Environment as Env
import qualified System.Posix.Internals

ssEnvVarName :: String
ssEnvVarName = "SERVER_STARTER_PORT"

data SSPort = SSPortTCP String CInt

serverPorts :: String -> [SSPort]
serverPorts cs = go cs
  where
    go cs = let (portFd, cs') = break (== ';') cs
                left = case cs' of ';' : cs'' -> go cs''
                                   otherwise  -> []
            in ssport portFd : left
    ssport portFd = let (port, '=' : fd) = break (== '=') portFd
                    in SSPortTCP port (read fd)

listenSSPort :: SSPort -> IO Socket.Socket
listenSSPort (SSPortTCP _ fd) = do
  -- See https://github.com/haskell/network/blob/master/Network/Socket.hsc
  System.Posix.Internals.setNonBlockingFD fd True

  Socket.mkSocket
    fd
    Socket.AF_INET
    Socket.Stream
    Socket.defaultProtocol
    Socket.Listening

listenAll :: IO [Socket.Socket]
listenAll = do
  ssenv <- Env.getEnv ssEnvVarName
  let ssports = serverPorts ssenv
  mapM listenSSPort ssports
