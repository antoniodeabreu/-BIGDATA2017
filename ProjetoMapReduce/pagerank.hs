{-|
Module      : PageRank 
Description : PageRank in MapReduce
Copyright   : (c) Antonio de Abreu Batista Júnior, 2017
License     : GPL-3
Maintainer  : antonio.batista@ufma.br
Calculates the PageRank algorithm over distributed system using
MapReduce.
-}

module Main where
import Control.Arrow ((&&&))
import Data.List
import Data.Function


-- The shuffle step puts all key-value pairs with the same key on one machine -> groupBykey does the shuffle step
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
reducer (m, xs) = (m, ( demp (sum ps), concat ns))
 where ps = map fst $ map snd xs
       ns = map snd $ map snd xs

demp :: Double -> Double
demp x = (1-0.85)/5 + 0.85 *x



reducer2 :: [( Int , [(Int , (Double,[Int]))] )] -> [(Int, (Double, [Int]))]
reducer2 x =  map reducer x   

pageRankList y = iterate pageRank y

pageRank :: [(Int, (Double, [Int]))] -> [(Int, (Double, [Int]))]
pageRank x = reducer2 $ groupBykey f2  $ mapper2  x
   
main :: IO ()
main = do
 let x = [(1,(0.2,[2,4])),(2,(0.2,[5,3])),(3,(0.2,[4])),(4,(0.2,[5])),(5,(0.2,[1,2,3]))]
     

 print (  take 2 (pageRankList  x) !! 1 ) --11.5
