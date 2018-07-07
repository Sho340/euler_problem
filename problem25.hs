main :: IO ()
main = print $ getIndexForDigit fibs 1000

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

getIndexForDigit :: [Integer] -> Int -> Int
getIndexForDigit fibs digit = length $ takeWhile (isLtEq digit) fibs

isLtEq :: Int -> Integer -> Bool
isLtEq  cur val = length (show val) < cur
