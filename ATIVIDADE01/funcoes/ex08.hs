module Main where


factorial 0 = 1                 -- zero step
factorial n = n * factorial (n-1)    -- induction step


-- coeficientes binomiais
binomial n k = factorial n `div` (factorial k * factorial (n-k))


pascal :: Int->Int->Int
pascal row entry = binomial row entry

main :: IO ()
main = do
	
		
        print (pascal 8 3) --- 56 
