module Main where

digitSum :: Int -> Int
digitSum 0 = 0
digitSum (n) = mod n 10 + digitSum (div n 10)


main :: IO ()
main = do
	
     print (digitSum 33333) --- 15 
