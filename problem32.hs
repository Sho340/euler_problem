import Data.List
import Control.Monad

main :: IO ()
main = do
  let fiveNums = filter (\x -> length x == 5) $ subsequences ['1'..'9']
  let mulps = nub $ join $ map getPerm fiveNums
  print $ sum mulps

getPerm :: String -> [Int]
getPerm nums = let m = filter (\x -> length (fst x) `elem` [4,3])
                                     $ overComb nums
               in join $ map (\x -> getMultip (fst x) (snd x)) m

overComb :: String -> [(String, String)]
overComb str = get str ("","")
  where
    get "" (str1, str2) = [(str1, str2)]
    get (x:xs) (str1, str2) = get xs (x:str1, str2) ++ get xs (str1, x:str2)

getMultip :: String -> String -> [Int]
getMultip mul1 mul2 = let muls1 = map (\x -> read x::Int) $ permutations mul1
                          muls2 = map (\x -> read x::Int) $ permutations mul2
                          result = (*) <$> muls1 <*> muls2
                      in filter (\x-> isMultiplicand (mul1 ++ mul2 ++ show x)) result
  where isMultiplicand x = (length x == 9) && (length (intersect ['1'..'9'] x) == 9)
