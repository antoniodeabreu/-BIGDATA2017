{-|
Module      : PageRank
Description : PageRank using MapReduce
Copyright   : (c) Antonio de Abreu, 2017
License     : GPL-3
Maintainer  : antonio.batista@ufma.br
Parallel PageRank using MapReduce.
-}

module Main where

import Control.Parallel.Strategies
import qualified Data.HashMap.Strict as M
import Data.Char (toLower, isAlphaNum)
import Data.List (foldl', groupBy,sortBy, concat)
import Data.List.Split (splitOn, chunksOf)
import Data.Hashable (Hashable)
import System.Environment
import Control.Arrow ((&&&))
import Data.Function



type Op a = (a -> a -> a)    -- | Function of two inputs
type Mapper k a b = k -> (b, a) -- | Function that maps a key to a key value tuple


-- |'combiner' combines a list of tuples to a HashMap using 
-- a specified operator
combiner :: (Hashable b0, Eq b0)
         => Op a0 
         -> [(b0,a0)] 
         -> M.HashMap b0 a0
combiner f m   = M.fromListWith f m

-- | 'mapper' maps a list of keys to a HashMap of key-values 
-- using a mapper function and combining with an operator
mapper :: (Hashable b0, Eq b0) 
       => Op a0 
       -> Mapper k0 a0 b0
       -> [k0] 
       -> M.HashMap b0 a0
mapper combfun f job = combiner combfun $ map f job

-- |'reducer' reduces the output of the 'mapper' 
-- to produce the desired output
reducer :: (Hashable k0, Eq k0) 
        => Op a0 
        -> [M.HashMap k0 a0] 
        -> M.HashMap k0 a0
reducer f m  = foldl' (M.unionWith f) M.empty m


-- | pre processamento 
{--
 Gera a entrada, para cada nó lido (node1, (pagerank, [node4, node2])) é gerado (node4,(pagerank/2, []), (node2,(pagerank/2, []), (node1, (0, [node4, node2])). Resumindo seria um lista de nós gerados pelo mapper de cada nó)
--}

parseFile :: String -> [(Int, Int)]
parseFile file = map parseLine (lines file) --`using` parList rdeepseq
 where
  parseLine l = convert $ map toInt (words l)
  convert [x,y] = (x,y)
  toInt w = read w :: Int
  

trans :: [(Int, [(Int, Int)])] -> [(Int,( Double, [Int]))]
trans x = concat lists
 where 
  lists =  map aux chunks `using` parList rdeepseq
  aux x = map transform x
  chunks = chunksOf 5000 x
    


transform :: (Int, [(Int, Int)]) -> (Int,( Double, [Int]) )
transform (x, ns) = (x, (1/281903, ps )) 
  where 
   ps = map snd ns

join :: [(Int, (Double, [Int]))] -> [(Int, (Double, [Int]))]
join x = concat $ map create x 

create :: (Int, (Double, [Int])) -> [(Int, (Double, [Int]))]
create (m, (p, ns))  = [(n, ((/) p x , []))| n<-ns ] ++ [(m, (p, []))| x==0 ] ++ [(m, (0, ns))]
 where
  x=fromIntegral $ length ns

groupBykey :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBykey f = map (fst . head &&& map snd)
                   . groupBy ((==) `on` fst)
                   . sortBy (compare `on` fst)
                   . map (f &&& id)

f1 :: (Int, Int) -> Int
f1 (x, _) = x

-- Fim pre-processamento


-- | pagerank em MapReduce  
pagerank :: Int -> [(Int, (Double, [Int]))] -> M.HashMap Int (Double, [Int])
pagerank n nl = reducer myreducer mapped
 where
  mapped     = map mapfun workers `using` parList rdeepseq
  mapfun     = mapper myreducer mymapper
  workers    = chunksOf n nl


mymapper :: (Int, (Double, [Int])) -> (Int, (Double, [Int]))
mymapper m = m   

myreducer :: (Double, [Int])-> (Double, [Int]) -> (Double, [Int])
myreducer (m1, xs1)  (m2, xs2) = (demp (m1 + m2), xs1 ++ xs2)


demp :: Double -> Double
demp x = (1-0.85)/281903 + 0.85 *x



loop n x = iterate (f n) x

f :: Int->[(Int,(Double,[Int]))]->[(Int,(Double,[Int]))]
f n x = M.toList $ pagerank n $ join x

main = do	
  --let x = [(1,(0.2,[2,4])),(2,(0.2,[5,3])),(3,(0.2,[4])),(4,(0.2,[5])),(5,(0.2,[1,2,3]))]
  --print (take 10000 (loop x) !! 9999)

 args <- getArgs
 file <- readFile (args !! 0)
 --pre processamento
 let dataset = join $ trans $ groupBykey f1 $ parseFile file
 -- pega o resultado da 20 iteração. A lista de entrada é quedrada em 10000 pedaços
 print (take 20 (loop 10000 dataset) !! 19)

  
