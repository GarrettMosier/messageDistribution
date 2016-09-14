module Main where

import Lib
import Options.Applicative
import Text.Read




getDuration (CommandLineRequest duration _ _) = duration
getGracePeriod (CommandLineRequest _ gracePeriod _) = gracePeriod
getSeed (CommandLineRequest _ _ seed) = seed


commandLineParser :: Parser CommandLineRequest
commandLineParser = CommandLineRequest <$>
    option auto (long "send-for" <> metavar "SENDING_DURATION" <> help "How long to send messages in first part of program execution") <*>
    option auto (long "wait-for" <> metavar "GRACE_PERIOD" <> help "How long to wait after the batch of messages was sent until calculation time.") <*> 
    option auto (long "with-seed" <> metavar "SEED" <> help "The seed to be used in the random message generation") 


options = info (helper <*> commandLineParser)
    ( fullDesc 
        <> progDesc "Send messages to all nodes in a cluster"
        <> header "MessagePass - A message passing program")


main :: IO ()
main = execParser options >>= bigFunc
