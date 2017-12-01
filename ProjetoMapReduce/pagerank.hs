module Main where
import Control.Arrow ((&&&))
import Data.List
import Data.Function


groupBykey :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBykey f = map (fst . head &&& map snd)
                   . groupBy ((==) `on` fst)
                   . sortBy (compare `on` fst)
                   . map (f &&& id)

f2 :: (Int, (Double, [Int])) -> Int
f2 (x, _) = x

mapper :: (Int, (Double, [Int])) -> [(Int, (Double, [Int]))]
mapper (m, (p, ns))  = [(n, ((/) p x , []))| n<-ns ] ++ [(m, (p, []))| x==0 ] ++ [(m, (0, ns))]
 where
  x=fromIntegral $ length ns

mapper2 :: [(Int, (Double, [Int]))] -> [(Int, (Double, [Int]))]
mapper2 x = concat $ map mapper x 


reducer :: ( Int , [(Int , (Double,[Int]))] )-> (Int, (Double, [Int]))
reducer (m, xs) = (m, (sum ps, concat ns))
 where ps = map fst $ map snd xs
       ns = map snd $ map snd xs

reducer2 :: [( Int , [(Int , (Double,[Int]))] )] -> [(Int, (Double, [Int]))]
reducer2 x =  map reducer x   

    
main :: IO ()
main = do
 let x = [(1,(0.10000000000000002,[2,4])),(2,(0.13333333333333336,[5,3])),(3,(0.18333333333333335,[4])),(4,(0.2,[5])),(5,(0.3833333333333334,[1,2,3]))]


     y=mapper2 x

 print (reducer2 $ groupBykey f2 y) --11.5
