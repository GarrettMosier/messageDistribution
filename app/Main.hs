module Main where

import Lib
import Options.Applicative


data CommandLineRequest = CommandLineRequest { durationToSendMessages :: String, gracePeriod :: String, seed :: String}


commandLineParser :: Parser CommandLineRequest
commandLineParser = CommandLineRequest <$>
    strOption (long "send-for" <> metavar "SENDING_DURATION" <> help "How long to send messages in first part of program execution") <*>
    strOption (long "wait-for" <> metavar "GRACE_PERIOD" <> help "How long to wait after the batch of messages was sent until calculation time.") <*> 
    strOption (long "with-seed" <> metavar "SEED" <> help "The seed to be used in the random message generation") 
    

options = info (helper <*> commandLineParser)
    ( fullDesc 
        <> progDesc "Send messages to all nodes in a cluster"
        <> header "MessagePass - A message passing program")


main :: IO ()
main = bigFunc 1 1 1
