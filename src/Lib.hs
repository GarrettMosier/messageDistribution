
module Lib where

import Control.Distributed.Process
import Control.Distributed.Process

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg


bigFunc :: IO ()
bigFunc = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    -- Spawn another worker on the local node
    echoPid <- spawnLocal $ forever $ do
      -- Test our matches in order against each message in the queue
      receiveWait [match logMessage, match replyBack]

    -- The `say` function sends a message to a process registered as "logger".
    -- By default, this process simply loops through its mailbox and sends
    -- any received log message strings it finds to stderr.


{-
    say "send some messages!"
    send echoPid "hello"


    self <- getSelfPid
    send echoPid (self, "hello")
-}

    self <- getSelfPid
    send self (100 :: Int)
    send self (20 :: Int)
    sup <- expect :: Process Int
    yo <- expect :: Process Int
    liftIO $ print $ suma [sup, yo]


{-
    -- `expectTimeout` waits for a message or times out after "delay"
    m <- expectTimeout 1000000
    case m of
      -- Die immediately - throws a ProcessExitException with the given reason.
      Nothing  -> die "nothing came back!"
      Just s -> say $ "got " ++ s ++ " back!"

    -- Without the following delay, the process sometimes exits before the messages are exchanged.
    liftIO $ threadDelay 2000000
-}

suma :: (Num a, Enum a) => [a] -> a 
suma = weightedAverage [1..] 


weightedAverage :: (Num a ) => [a] -> [a] -> a
weightedAverage weights values = foldr (+) 0 $ zipWith (*) weights values 
