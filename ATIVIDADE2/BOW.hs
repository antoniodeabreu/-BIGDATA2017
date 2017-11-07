import Control.DeepSeq
import Control.Parallel.Strategies
import qualified Data.HashMap.Strict as M
import Data.Char (toLower, isAlphaNum)
import Data.List (foldl', nub, tails, intercalate, concat)
import Data.Hashable (Hashable)
import Data.List.Split (chunksOf)

type TF = M.HashMap String Double

-- |'tokenize' a line to BoW
tokenize :: String -> [String]
tokenize line = preProcess $ words line
  where
    preProcess     = (filter moreThanTwo) . (map normalize)
    normalize word = map toLower $ filter isAlphaNum word
    moreThanTwo l  = length l > 2

-- |'binarize' the corpus of BoW
binarize :: [[String]] -> [TF]
binarize corpus = map binVec corpus
  where
   binVec line = M.fromList $ zip line [1,1..]

-- |
getTokens :: String -> [[String]]
getTokens text = notEmpty $ map tokenize $ lines text
 where
  notEmpty = filter (not . null)



-- |'tf' generates the term frequency of a BoW
tf :: [[String]] -> [TF]
tf corpus = map countWords corpus
  where
    countWords doc = M.map (\v -> v / (len doc)) $ M.fromListWith (+) $ zip doc [1,1..]
    len d = fromIntegral $ length d

-- |tfPAR parallel version
tfPAR :: [[String]] -> [TF]
tfPAR l = concat lists
  where 
   lists = map tf chunks `using` parList rdeepseq
   chunks = chunksOf 1000 l


-- | 'df' calculates document frequency of words in a dictionary
df :: [TF] -> TF
df corpus = foldl' (M.unionWith (+)) M.empty corpus

-- |dfPAR parallel version
dfPAR :: [TF] -> TF
dfPAR l = foldl' (M.unionWith (+)) M.empty lists
  where 
   lists = map df chunks `using` parList rdeepseq
   chunks = chunksOf 600 l



-- |'tfidf' of a document
tfidf :: [TF] -> TF -> [TF]
tfidf tf' df' = map calcTFIDF tf' 
  where
   calcTFIDF t = M.mapWithKey calc t
   calc k v = v * log (n / (getDF k))
   getDF t = M.lookupDefault 0 t df'
   n = fromIntegral $ length tf'



-- |'main'
main :: IO ()
main = do
 text <- readFile "/home/antonio/haskell/tf/pg"
 let
  corpus = getTokens text
  tf' = tfPAR corpus
  df' = dfPAR $ tf'
  tfidf' = tfidf tf' df'
  
 print (tfidf')
  
-- | As versões paralelas do tf (tfPAR) e df (dfPAR) aceleram o cálculo do  
-- | tfidf  aproximadamente 1.05 vezes.	


