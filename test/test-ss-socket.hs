{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import System.Environment
import System.Posix.Process
import Network.ServerStarter.Socket
import Test.HUnit

main :: IO ()
main = do
  runTestTT testListenAll
  return ()

testListenAll :: Test
testListenAll = TestCase $ do
  s <- socket AF_INET Stream defaultProtocol
  bind s $ SockAddrInet aNY_PORT iNADDR_ANY
  listen s 1
  let fd = fdSocket s
  addr@(SockAddrInet port _) <- getSocketName s
  let env = show port ++ "=" ++ show fd
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
