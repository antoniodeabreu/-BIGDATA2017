module Main where


bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

anosbissexto = [ano |  ano <- [1..2017], bissexto ano == True] -- |exercicio07 


ultimosN :: Int -> [a] -> [a]
ultimosN n xs = let m = length xs in drop (m-n) xs

--splitAt' = \n -> \xs -> (take n xs, drop n xs)



main :: IO ()
main = do
 


  print (take 10 anosbissexto) -- |exercicio08 os dez primeiros 
  print ( ultimosN 10 anosbissexto) -- |exercicio09 os dez últimos

 -- let m = length anosbissexto 
 -- print ( splitAt' m anosbissexto) 

