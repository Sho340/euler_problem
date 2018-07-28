import           Data.List

main :: IO()
main = do
  let tupleList = [(x,getCirculationClauseLength x) | x <- [3..999],  mod x 2 /= 0 , mod x 5 /= 0 ]
  print $ fst $ foldl (\acc x -> if snd acc < snd x then x else acc) (0,0) tupleList

getCirculationClauseLength :: Int -> Int
getCirculationClauseLength divisor = getModNum 1 [1]
  where
    getModNum :: Int -> [Int] -> Int
    getModNum modNum list
      | not (null (list \\ ([0..divisor]::[Int])))  = let len = length list
                                                         in  case elemIndices modNum list of
                                                              [a,b] -> b-a
                                                              []    -> 0
      | otherwise =  let dividend = getCanDividedNum modNum divisor
                         newMod   = mod dividend divisor
                     in  getModNum newMod (newMod : list)



getCanDividedNum :: Int -> Int -> Int
getCanDividedNum dividend divisor
  | dividend >= divisor = dividend
  | otherwise           = getCanDividedNum ( dividend * 10 ) divisor
