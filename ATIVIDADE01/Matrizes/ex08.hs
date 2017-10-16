module Main where

type Matrix = [[Int]]


elemMatrix :: Int -> Int -> Int -> Int -> Matrix
-- elemMatrix n i j v é uma matriz elementar nxn
-- com v na posição (i,j) 
elemMatrix n i j v = [ [ entry row column | column <- [1..n] ] | row <- [1..n] ]
 where 
  entry x y
   | x == y           = 1
   | x == i && y == j = v
   | otherwise       = 0

-- |controe a matriz identidade de tamanho n
idMatrix :: Int -> Matrix    -- | exercício 1
idMatrix n = elemMatrix n 1 1 1


-- | obtém a diagonal principal
mainDiagonal :: (Num a) => [[a]] -> [a]
mainDiagonal []     = []
mainDiagonal (x:xs) = head x : mainDiagonal (map tail xs)

-- |soma os elementos de uma lista 
soma :: Num a => [a] -> a
soma [] = 0
soma (x : xs) = x + soma xs

-- |soma os elementos da diagonal principal
somaDiagonal :: Num a => [[a]] -> a  -- | exercício 2
somaDiagonal mtx = soma$mainDiagonal mtx



main :: IO ()
main = do
	
	print (idMatrix 5) -- [[1,0],[0,1]]
        print (somaDiagonal(idMatrix 5)) --- 5 
 

 
