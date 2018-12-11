import Data.Char

main :: IO ()
main = do
    print $ sum $ filter isEqSumOf5Pow [2..(9 ^ 5) * 6]

isEqSumOf5Pow :: Int -> Bool
isEqSumOf5Pow n = let list = map digitToInt $ show n
                      sumOf5Pow = sum $ map (\x -> (^) x 5) list
                  in n == sumOf5Pow
