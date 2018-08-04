main :: IO()
main = print $ getDistinctPowLength 100

getDistinctPowLength :: Int -> Int
getDistinctPowLength n = length $ getDistinctPows $ indexLogNums n
  where
    indexLogNums num = [(x,y)|x <- [2..num],y <- [2..num]]
    getDistinctPows :: [(Int,Int)] -> [Integer]
    getDistinctPows = foldl (\acc l -> let pow = (^) (fromIntegral(fst l)::Integer) (fromIntegral(snd l)::Integer) in
                                if pow `elem` acc then
                                  acc
                                else
                                  pow : acc
                              ) []
