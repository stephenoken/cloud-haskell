\module{Node}

Module \highlighttt{Node} represents an indivudla
process

Import Modules
\begin{code}
module Node where

import Control.Concurrent(threadDelay)
import Control.Monad(forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)
\end{code}

The node's functions
\begin{code}
replayBack :: (ProcessId,String) -> Process()
replayBack (sender, msg) = send sender msg

logMessage :: String -> Process()
logMessage msg = say $ "handling " ++ msg
\end{code}

IO block
\begin{code}
main :: IO ()
main = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    -- Spawn another worker on the local node
    echoPid <- spawnLocal $ forever $ do
      -- Test our matches in order against each message in the queue
      receiveWait [match logMessage, match replayBack]
      -- The `say` function sends a message to a process registered as "logger".
      -- By default, this process simply loops through its mailbox and sends
      -- any received log message strings it finds to stderr.
    say "send some messages!!"
    send echoPid "Hello"
    self <- getSelfPid
    send echoPid (self,"hello")
    -- `expectTimeout` waits for a message or times out after "delay"
    m <- expectTimeout 100000000000000000
    case m of
      -- Die immediately - throws a ProcessExitException with the given reason.
      Nothing -> die "Nothing came back!!"
      Just s -> say $ "got " ++ s ++ " back"
\end{code}
Note that we’ve used receiveWait this time around to get a message. receiveWait
and similarly named functions can be used with the Match data type to provide a
range of advanced message processing capabilities. The match primitive allows you
to construct a “potential message handler” and have it evaluated against received
(or incoming) messages. Think of a list of Matches as the distributed equivalent
of a pattern match. As with expect, if the mailbox does not contain a message
that can be matched, the evaluating process will be blocked until a message
arrives which can be matched.

In the echo server above, our first match prints out whatever string it
receives. If the first message in our mailbox is not a String, then our second
match is evaluated. Thus, given a tuple t :: (ProcessId, String), it will send
the String component back to the sender’s ProcessId. If neither match succeeds,
the echo server blocks until another message arrives and tries again.
