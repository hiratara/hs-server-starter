{-# LANGUAGE OverloadedStrings, CPP #-}
module Main (main) where

import Data.List
import Foreign.C.Types
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import System.Environment
import System.Posix.Process
import Network.ServerStarter.Socket
import Test.HUnit
import System.IO.Temp

ioFdSocket :: Socket -> IO CInt
#if MIN_VERSION_network(3, 1, 0)
ioFdSocket = unsafeFdSocket
#else
ioFdSocket = return . fdSocket
#endif

main :: IO ()
main = do
  runTestTT . test $
    [ testListenAll portenv
    , testListenAll hostportenv
    , testListenAllIpv6 portenv
    , testListenAllIpv6 host6portenv
    , testListenAllUnix
    ]
  return ()
  where
    portenv      p _ fd = show p ++ "=" ++ show fd
    hostportenv  p h fd = h ++ ":" ++ show p ++ "=" ++ show fd
    host6portenv p h fd = "[" ++ h ++ "]:" ++ show p ++ "=" ++ show fd

testListenAll :: (PortNumber -> String -> CInt -> String) -> Test
testListenAll makeEnv = TestCase $ do
  s <- socket AF_INET Stream defaultProtocol
  bind s $ SockAddrInet 0 (tupleToHostAddress (0, 0, 0, 0))
  listen s 1
  fd <- ioFdSocket s
  addr@(SockAddrInet port _) <- getSocketName s
  (Just host, _) <- getNameInfo [] True False addr
  let env = makeEnv port host fd
  setEnv "SERVER_STARTER_PORT" env
  forkProcess child
  s' <- socket AF_INET Stream defaultProtocol
  connect s' addr

  let pingStr = "GOGOGO"
  send s' pingStr
  got <- recv s' 1024
  pingStr @=? got

testListenAllUnix :: Test
testListenAllUnix = TestCase $ withSystemTempDirectory "ssstest" $ \tmpdir -> do
  s <- socket AF_UNIX Stream defaultProtocol
  let socketfile = tmpdir ++ "/test.sock"
      sockaddr   = SockAddrUnix socketfile
  bind s $ sockaddr
  listen s 1
  fd <- ioFdSocket s
  -- addr@(SockAddrInet port host) <- getSocketName s
  let env = socketfile ++ "=" ++ show fd
  setEnv "SERVER_STARTER_PORT" env
  forkProcess child
  s' <- socket AF_UNIX Stream defaultProtocol
  connect s' sockaddr

  let pingStr = "GOGOGO"
  send s' pingStr
  got <- recv s' 1024
  pingStr @=? got

testListenAllIpv6 :: (PortNumber -> String -> CInt -> String) -> Test
testListenAllIpv6 makeEnv = TestCase $ do
  s <- socket AF_INET6 Stream defaultProtocol
  bind s $ SockAddrInet6 0 0 (0, 0, 0, 0) 0
  listen s 1
  fd <- ioFdSocket s
  addr@(SockAddrInet6 port _ whost _) <- getSocketName s
  let host = hoststr whost
  let env = makeEnv port host fd
  setEnv "SERVER_STARTER_PORT" env
  forkProcess child
  s' <- socket AF_INET6 Stream defaultProtocol
  connect s' addr

  let pingStr = "GOGOGO"
  send s' pingStr
  got <- recv s' 1024
  pingStr @=? got
  where
    hoststr w = let (w1, w2, w3, w4, w5, w6, w7, w8) = hostAddress6ToTuple w
                 in intercalate ":" $ map show [w1, w2, w3, w4, w5, w6, w7, w8]

child :: IO ()
child = do
  (s:_) <- listenAll

  (s', _) <- accept s
  got <- recv s' 1024
  send s' got
  return ()
