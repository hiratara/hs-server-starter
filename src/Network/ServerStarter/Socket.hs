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

{-# LANGUAGE Strict, CPP #-}
module Network.ServerStarter.Socket
    ( listenAll
    ) where

import qualified Data.Char as Char
import Foreign.C.Types (CInt)
import qualified Network.Socket as Socket
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Posix.Internals
import qualified Text.Read as Read

ssEnvVarName :: String
ssEnvVarName = "SERVER_STARTER_PORT"

data SSPort = SSPort String CInt

makeSocket :: CInt -> Socket.Family -> IO Socket.Socket
#if MIN_VERSION_network(3, 0, 0)
makeSocket fd _ = Socket.mkSocket fd
#else
makeSocket fd fam = Socket.mkSocket
  fd
  fam
  Socket.Stream
  Socket.defaultProtocol
  Socket.Listening
#endif

serverPorts :: String -> [SSPort]
serverPorts cs = go cs
  where
    go cs = let (portFd, cs') = break (== ';') cs
                left = case cs' of ';' : cs'' -> go cs''
                                   otherwise  -> []
            in ssport portFd : left
    ssport portFd = let (str, '=' : fd) = break (== '=') portFd
                        fdcint = read fd :: CInt
                    in SSPort str fdcint

listenSSPort (SSPort _ fd) = do
  -- See https://github.com/haskell/network/blob/master/Network/Socket.hsc
  System.Posix.Internals.setNonBlockingFD fd True

  -- We always use AF_INET. See perl implementations:
  -- https://metacpan.org/source/KAZUHO/Starlet-0.31/lib/Plack/Handler/Starlet.pm#L26-30
  makeSocket fd Socket.AF_INET

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
