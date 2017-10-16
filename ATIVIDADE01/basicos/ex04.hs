-- | Exercício 04 
module Main where

 

mult35 :: Integer -> Bool
mult35 x =  mult3 x || mult5 x
 where
  
  mult3 x = (x`mod`3==0) 
  mult5 x = (x`mod`5==0)

main :: IO ()
main = do
 
 print (mult35 45) --True
 print (mult35 13) --False
 	 
