
module Lib
    ( someFunc
    ) where

import Control.Distributed.Process
import Control.Distributed.Process

import Network.Transport.TCP (createTransport, defaultTCPParameters)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
