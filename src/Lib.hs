
module Lib where

import Control.Distributed.Process

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Distributed.Process.Node
import Control.Distributed.Process.Extras.Time
import Control.Distributed.Process.Extras.Timer
import System.Random

import MathUtil
import MessageUtil

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg


data CommandLineRequest = CommandLineRequest { durationToSendMessages :: TimeToSendMessages, gracePeriod :: GracePeriod, seed :: Seed}



serverLocations = [("127.0.0.1", "10501"), ("127.0.0.1", "10503")]
transportLocations = fmap (\(host, port) -> createTransport host port defaultTCPParameters) serverLocations

bigFunc :: CommandLineRequest -> IO ()
bigFunc (CommandLineRequest timeToSendMessages gracePeriod seed) = do
  let messagesToSendOutForever = randomStream seed :: Messages

  Right t <- transportLocations !! 0 -- TODO Have it work for all values
  node <- newLocalNode t initRemoteTable
  runProcess node $ do

    self <- getSelfPid

    spamMessagesPid <- sendMessagesForever messagesToSendOutForever [self] 
    killAfter (seconds timeToSendMessages) spamMessagesPid "Time to be done" 


    -- TODO Make sure I don't start two process for reading messages
    let blah = expectMessages
    --expectingPid <- spawnLocal $ fmap (\x -> ()) blah
    --killAfter (seconds (timeToSendMessages + gracePeriod)) expectingPid "Stop collecting messages please" 


    sup <- expect :: Process Float 
    yo <- expect :: Process Float
    hey <- expect :: Process Float
    liftIO $ print $ suma [sup, yo, hey]


    let summed = fmap suma blah :: Process Float
    stuff <- summed
    liftIO $ print stuff--summed
    liftIO $ print "Done"

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
