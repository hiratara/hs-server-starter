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
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Posix.Internals
import qualified Text.Read as Read

ssEnvVarName :: String
ssEnvVarName = "SERVER_STARTER_PORT"

data SSPort = SSPort Socket.Family String CInt

serverPorts :: String -> [SSPort]
serverPorts cs = go cs
  where
    go cs = let (portFd, cs') = break (== ';') cs
                left = case cs' of ';' : cs'' -> go cs''
                                   otherwise  -> []
            in ssport portFd : left
    ssport portFd = let (str, '=' : fd) = break (== '=') portFd
                        fdcint = read fd :: CInt
                        fam    = socketFamily str
                    in SSPort fam str fdcint

socketFamily :: String -> Socket.Family
socketFamily str = if looksLikeHostPort str
                   then Socket.AF_INET
                   else Socket.AF_UNIX

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

listenSSPort (SSPort fam _ fd) = do
  -- See https://github.com/haskell/network/blob/master/Network/Socket.hsc
  System.Posix.Internals.setNonBlockingFD fd True

  Socket.mkSocket
    fd
    fam
    Socket.Stream
    Socket.defaultProtocol
    Socket.Listening

checkUnixPort :: SSPort -> IO Bool
checkUnixPort (SSPort Socket.AF_UNIX str _) = Dir.doesPathExist str
checkUnixPort _ = return True

{-|
The 'listenAll' function takes a file descriptor from the environment variable
@SERVER_STARTER_PORT@ and returns it wrapped in 'Network.Socket.Socket' type.
With return value as an argument, you can call 'Network.Socket.accept' as it is.
-}
listenAll :: IO [Socket.Socket]
listenAll = do
  ssenv <- Env.getEnv ssEnvVarName
  let ssports = serverPorts ssenv
  okUnixPort <- mapM checkUnixPort ssports
  if and okUnixPort then return()
                    else error "unix domein socket not found"
  mapM listenSSPort ssports
