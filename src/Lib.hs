
module Lib where

import Control.Distributed.Process

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Distributed.Process.Node
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


sendMessagesForever :: Messages -> ProcessId -> Process ProcessId
sendMessagesForever messages recipient = spawnLocal $ sendMessages messages recipient

sendMessages :: Messages -> ProcessId -> Process ()
sendMessages messages recipient = mapM_ (send recipient) messages 


expectMessagesUtil :: Messages -> Process Messages
expectMessagesUtil li = do
  receivedMessage <- expect :: Process Float
  expectMessagesUtil $ li ++ [receivedMessage]


expectMessages :: Process Messages
expectMessages = expectMessagesUtil []


serverLocations = [("127.0.0.1", "10501")]
transportLocations = fmap (\(host, port) -> createTransport host port defaultTCPParameters) serverLocations

bigFunc :: CommandLineRequest -> IO ()
bigFunc (CommandLineRequest timeToSendMessages gracePeriod seed) = do
  let messagesToSendOutForever = randomStream seed :: Messages

  Right t <- transportLocations !! 0 -- TODO Have it work for all values
  node <- newLocalNode t initRemoteTable
  runProcess node $ do

    self <- getSelfPid

    spamMessagesPid <- sendMessagesForever messagesToSendOutForever self
    killAfter (seconds timeToSendMessages) spamMessagesPid "Time to be done" 


    -- TODO Make sure I don't start two process for reading messages
    let blah = expectMessages
    expectingPid <- spawnLocal $ fmap (\x -> ()) blah
    killAfter (seconds (timeToSendMessages + gracePeriod)) expectingPid "Stop collecting messages please" 


    sup <- expect :: Process Float 
    yo <- expect :: Process Float
    liftIO $ print $ suma [sup, yo]

{-
    let summed = fmap suma blah :: Process Float
    stuff <- summed
    liftIO $ print stuff--summed
-}

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
