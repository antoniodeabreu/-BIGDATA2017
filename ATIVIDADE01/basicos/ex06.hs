-- | Exercício 06 
module Main where

 

div2d :: Integer -> Double

div2d x =  fromIntegral(x) / 2
 

main :: IO ()
main = do
 
 print (div2d 23) --11.5
