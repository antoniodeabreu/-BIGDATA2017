module Main where

fibonacci :: Floating a => a -> a

fibonacci n = (ro**n - (1-ro)**n) / sqrt 5
 where 
  ro = (1 + sqrt 5) / 2

lista = [fibonacci x | x <- [0..] ]


main :: IO ()
main = do
	
		
        print (take 10 lista) --- True
