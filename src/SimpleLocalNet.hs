import System.Environment(getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node(initRemoteTable,runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad(forever,forM_)

-- main :: IO
main = do
  [host,port] <-getArgs

  backend <- initializeBackend host port initRemoteTable
  node <- newLocalNode backend
  peers <- findPeers backend 9000000
  runProcess node $ forM_ peers (\ peer -> nsendRemote peer "echo server" "Hello!")
