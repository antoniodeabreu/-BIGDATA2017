module Main where


factorial 0 = 1                 -- zero step
factorial n = n * factorial (n-1)    -- induction step


-- coeficientes binomiais
binomial n k = factorial n `div` (factorial k * factorial (n-k))


main :: IO ()
main = do
	
		
        print (binomial 6 2) --- 15 
 
