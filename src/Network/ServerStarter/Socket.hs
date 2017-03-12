{-|
Module      : Network.ServerStarter.Socket
Description : Write a server supporting Server::Starter's protocol in Haskell
Copyright   : Copyright (C) 2017- hiratara
License     : GPL-3
Maintainer  : hiratara@cpan.org
Stability   : experimental
Provides a utility to write server program which can be called via Perl's
<https://github.com/kazuho/p5-Server-Starter start_server> program using
Haskell.

Since the 'listenAll' function returns a listened 'Network.Socket.Socket',
please call 'Network.Socket.accept' on it.

@
  import qualified Network.ServerStarter.Socket as Starter
  import qualified Network.Socket               as Socket
  import qualified Network.Wai                  as Wai
  import qualified Network.Wai.Handler.Warp     as Warp

  main :: IO ()
  main = do
    (socket:_) <- Starter.listenAll
    Socket.SockAddrInet port _ <- Socket.getSocketName socket
    let setting = Warp.setPort (fromEnum port)
                $ Warp.defaultSettings
    Warp.runSettingsSocket setting socket app

  app :: Wai.Application
  app = ...
@

Then run @start_server@ and access to @http://localhost:12345@ .

@
  $ start_server --port 12345 -- stack exec server-starter-warp-example
@

-}

{-# LANGUAGE Strict #-}
module Network.ServerStarter.Socket
    ( listenAll
    ) where

import Foreign.C.Types (CInt)
import qualified Network.Socket as Socket
import qualified System.Environment as Env
import qualified System.Posix.Internals
import qualified Text.Read as Read

ssEnvVarName :: String
ssEnvVarName = "SERVER_STARTER_PORT"

data SSPort = SSPortTCP  String CInt
            | SSPortUnix String CInt

serverPorts :: String -> [SSPort]
serverPorts cs = go cs
  where
    go cs = let (portFd, cs') = break (== ';') cs
                left = case cs' of ';' : cs'' -> go cs''
                                   otherwise  -> []
            in ssport portFd : left
    ssport portFd = let (str, '=' : fd) = break (== '=') portFd
                        fdcint = read fd :: CInt
                    in if looksLikeHostPort str
                       then SSPortTCP  str fdcint
                       else SSPortUnix str fdcint

looksLikeHostPort :: String -> Bool
looksLikeHostPort str =
  case mPort of
    Just _  -> True
    Nothing -> case mPort' of
                 Just _  -> True
                 Nothing -> False
  where
    mPort = Read.readMaybe str :: Maybe Socket.PortNumber
    (host, strPort) =  break (== ':') str
    mPort' = case strPort of
      ':' : strPort' -> Read.readMaybe strPort' :: Maybe Socket.PortNumber
      otherwise      -> Nothing

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

listenSSPort (SSPortUnix _ fd) = do
  -- See https://github.com/haskell/network/blob/master/Network/Socket.hsc
  System.Posix.Internals.setNonBlockingFD fd True

  Socket.mkSocket
    fd
    Socket.AF_UNIX
    Socket.Stream
    Socket.defaultProtocol
    Socket.Listening

{-|
The 'listenAll' function takes a file descriptor from the environment variable
@SERVER_STARTER_PORT@ and returns it wrapped in 'Network.Socket.Socket' type.
With return value as an argument, you can call 'Network.Socket.accept' as it is.
-}
listenAll :: IO [Socket.Socket]
listenAll = do
  ssenv <- Env.getEnv ssEnvVarName
  let ssports = serverPorts ssenv
  mapM listenSSPort ssports
