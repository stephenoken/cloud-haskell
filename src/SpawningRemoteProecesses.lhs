\module{SpawningRemoteProecesses}

Module \highlighttt{SpawningRemoteProecesses} represents an
process

We saw above that the behaviour of processes is determined by an action in the
Process monad. However, actions in the Process monad, no more serializable than
actions in the IO monad. If we can’t serialize actions, then how can we spawn
processes on remote nodes?

The solution is to consider only static actions and compositions thereof.
A static action is always defined using a closed expression (intuitively, an
expression that could in principle be evaluated at compile-time since it does
not depend on any runtime arguments). The type of static actions in Cloud
Haskell is Closure (Process a). More generally, a value of type Closure b
is a value that was constructed explicitly as the composition of symbolic
pointers and serializable values. Values of type Closure b are serializable,
even if values of type b might not. For instance, while we can’t in general send
actions of type Process (), we can construct a value of type
Closure (Process ()) instead, containing a symbolic name for the action, and
send that instead. So long as the remote end understands the same meaning for
the symbolic name, this works just as well. A remote spawn then, takes a static
action and sends that across the wire to the remote node.

Static actions are not easy to construct by hand, but fortunately Cloud Haskell
provides a little bit of Template Haskell to help. If f :: T1 -> T2 then

 $(mkClosure 'f) :: T1 -> Closure T2

 You can turn any top-level unary function into a Closure using mkClosure.
 For curried functions, you’ll need to uncurry them first (i.e. “tuple up”
 the arguments). However, to ensure that the remote side can adequately
 interpret the resulting Closure, you’ll need to add a mapping in a so-called
 remote table associating the symbolic name of a function to its value.
 Processes can only be successfully spawned on remote nodes of all these
 remote nodes have the same remote table as the local one.

We need to configure our remote table (see the API reference for more details)
and the easiest way to do this, is to let the library generate the relevant
code for us. For example:

sampleTask :: (TimeInterval, String) -> Process ()
sampleTask (t, s) = sleep t >> say s

remotable ['sampleTask]

The last line is a top-level Template Haskell splice. At the call site for
spawn, we can construct a Closure corresponding to an application of
sampleTask like so:

($(mkClosure 'sampleTask) (seconds 2, "foobar"))

The call to remotable implicitly generates a remote table by inserting a
top-level definition __remoteTable :: RemoteTable -> RemoteTable in our module
for us. We compose this with other remote tables in order to come up with a
final, merged remote table for all modules in our program:

\begin{code}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode transport myRemoteTable
  runProcess node $ do
    us <- getSelfNode
    _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
    pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
    liftIO $ threadDelay 2000000

\end{code}
