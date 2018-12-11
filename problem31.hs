main :: IO ()
main = print $ search total total

total :: Int
total = 200

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

search :: Int -> Int -> Int
search rest x
  | rest < 0  = 0
  | rest == 0 = 1
  | otherwise = let workCoins = filter (<=x) coins
                    rests = map (\c -> rest - c) workCoins
                in sum $ zipWith search rests workCoins
