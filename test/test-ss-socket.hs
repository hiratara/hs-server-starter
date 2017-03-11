{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Foreign.C.Types
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import System.Environment
import System.Posix.Process
import Network.ServerStarter.Socket
import Test.HUnit

main :: IO ()
main = do
  runTestTT . test $
    [ testListenAll (\p _ fd -> show p ++ "=" ++ show fd)
    , testListenAll (\p h fd -> show h ++ ":" ++ show p ++ "=" ++ show fd)
    ]
  return ()

testListenAll :: (PortNumber -> HostAddress -> CInt -> String) -> Test
testListenAll makeEnv = TestCase $ do
  s <- socket AF_INET Stream defaultProtocol
  bind s $ SockAddrInet aNY_PORT iNADDR_ANY
  listen s 1
  let fd = fdSocket s
  addr@(SockAddrInet port host) <- getSocketName s
  let env = makeEnv port host fd
  setEnv "SERVER_STARTER_PORT" env
  forkProcess child
  s' <- socket AF_INET Stream defaultProtocol
  connect s' addr

  let pingStr = "GOGOGO"
  send s' pingStr
  got <- recv s' 1024
  pingStr @=? got

child :: IO ()
child = do
  (s:_) <- listenAll

  (s', _) <- accept s
  got <- recv s' 1024
  send s' got
  return ()
