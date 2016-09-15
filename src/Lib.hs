
module Lib where

import Control.Distributed.Process
import Control.Distributed.Process

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Extras.Time
import Control.Distributed.Process.Extras.Timer
import System.Random

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg


type TimeToSendMessages = Int
type GracePeriod = Int
type Seed = Int
type Messages = [Float]


data CommandLineRequest = CommandLineRequest { durationToSendMessages :: TimeToSendMessages, gracePeriod :: GracePeriod, seed :: Seed}

randomStream :: Seed -> Messages
randomStream = randomRs (0 :: Float, 1) . mkStdGen 



--sendMessagesForever :: [Float] -> IO ()
sendMessagesForever messages recipient = spawnLocal $ sendMessages messages recipient

sendMessages :: Messages -> ProcessId -> Process ()
sendMessages messages recipient = mapM_ (send recipient) messages 


expectMessages :: [Float] -> Process [Float]
expectMessages li = do
  receivedMessage <- expect :: Process Float
  expectMessages $ li ++ [receivedMessage]

bigFunc :: CommandLineRequest -> IO ()
bigFunc (CommandLineRequest timeToSendMessages gracePeriod seed) = do
  let messagesToSendOutForever = randomStream seed :: Messages

  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    -- Spawn another worker on the local node
    echoPid <- spawnLocal $ forever $ do
      -- Test our matches in order against each message in the queue
      receiveWait [match logMessage, match replyBack]

    self <- getSelfPid

    spamMessagesPid <- sendMessagesForever messagesToSendOutForever self

    killAfter (seconds timeToSendMessages) spamMessagesPid "Time to be done" 


    -- send self (100 :: Float)
    --send self (20 :: Float)
    sup <- expect :: Process Float 
    yo <- expect :: Process Float
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
