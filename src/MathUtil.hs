module MathUtil where

suma :: (Num a, Enum a) => [a] -> a 
suma = weightedAverage [1..] 


weightedAverage :: (Num a ) => [a] -> [a] -> a
weightedAverage weights values = foldr (+) 0 $ zipWith (*) weights values 
