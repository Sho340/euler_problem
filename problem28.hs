main :: IO()
main = print $ (snd . getDiaNum) 1001

getDiaNum :: Int -> (Int,Int)
getDiaNum n = foldl (\acc e ->
                let val = sum $ map (+ fst acc) $ take 4 [e, e * 2..]
                in ( fst acc + e * 4 , snd acc + val ) ) (1,1) list
              where radius = n - 1
                    list = [2,4.. radius]
