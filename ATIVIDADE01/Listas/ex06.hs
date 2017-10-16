module Main where


collatz:: Int -> Int -- | exercício 6
collatz x 
 | x `mod` 2 == 0 = x `div` 2
 | otherwise = (3*x + 1)


collatzLen :: Int -> Int -- | exercício 7
collatzLen a0 = length $ takeWhile (/= 1) $ iterate collatz a0


main :: IO ()
main = do
	
	
        print (collatzLen 10) --- 6
