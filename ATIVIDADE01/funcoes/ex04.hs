module Main where

isPrime :: Integer->Bool
isPrime x = not $ any divisible $ takeWhile notTooBig [2..]
 where
  divisible y = x `mod`y == 0
  notTooBig y = y*y <= x



main :: IO ()
main = do
	
		
        print (isPrime 31) --- true 
 
