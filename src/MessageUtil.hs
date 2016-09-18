module MessageUtil where

import Control.Distributed.Process

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Distributed.Process.Node
import Control.Distributed.Process.Extras.Time
import Control.Distributed.Process.Extras.Timer
import System.Random

import Control.Monad

type TimeToSendMessages = Int
type GracePeriod = Int
type Seed = Int
type Messages = [Float]


randomStream :: Seed -> Messages
randomStream = randomRs (0 :: Float, 1) . mkStdGen 


sendMessagesForever :: Messages -> [ProcessId] -> Process ProcessId
sendMessagesForever messages recipients = spawnLocal $ sendMessages messages recipients

sendMessages :: Messages -> [ProcessId] -> Process ()
sendMessages messages recipients = mapM_ (\recipient -> mapM_ (send recipient) messages) recipients 


expectMessagesUtil :: Messages -> Process Messages
expectMessagesUtil li = replicateM 1000 act
  where act = expect :: Process Float
  --expectMessagesUtil $ li ++ [receivedMessage]


expectMessages :: Process Messages
expectMessages = expectMessagesUtil []
