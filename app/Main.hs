module Main where

import Lib
import Network.Transport.TCP
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad(forever)
main :: IO ()
main = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ forever $ do
    self <- getSelfPid
    -- get our own process id
    send self "Hello"
    hello <- expect :: Process String
    liftIO $ putStrLn hello
  return ()
  print "Test"
