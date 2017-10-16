module Main where

dotProduct :: Num a => [a] -> [a] -> a 
dotProduct xs ys = sum (zipWith (*) xs ys)


main :: IO ()
main = do
	let a=[1 , 2 ]
	let b=[1 , 3 ]
	
        print (dotProduct a b) --- 7
