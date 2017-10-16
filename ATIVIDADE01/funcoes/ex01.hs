module Main where


ehTriangulo :: (Num a, Ord a) => (a, a, a)-> Bool

ehTriangulo (x, y, z) 
 | x + y < z = error "Não forma um triângulo"
 |otherwise = True

main :: IO ()
main = do
	
		
        print (ehTriangulo (5, 5, 11)) --- True

