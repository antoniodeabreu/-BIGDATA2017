-- | Exercício 05 
module Main where

 

funcaoX :: Integer -> Bool
funcaoX x =  (x < -1) || ( x > 1 && (x`mod`2==0) )
 

main :: IO ()
main = do
 
 print (funcaoX 5) --False
 print (funcaoX (-3)) --True
