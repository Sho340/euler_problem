main :: IO()
main = print $ let maxPair = pair $ maxConPair 1000 in (*) (a maxPair) (b maxPair)

data Pair = Pair {a :: Int , b :: Int} deriving (Show)

data PairSet = PairSet {pair :: Pair , len :: Int } deriving (Show)

maxConPair :: Int -> PairSet
maxConPair lim = foldl getLongerSet (PairSet (Pair 0 0) 0) pairLenSet
  where lPrimes = takeWhile (<lim) primes
        mPrimes = map (*(-1)) lPrimes
        lmPrimes = lPrimes ++ mPrimes
        pairList = [Pair x y | x <- lmPrimes, y <- lmPrimes ]
        pairLenSet = zipWith PairSet pairList [0..]
        getLongerSet p1 p2 = if isPrime (quadratic (pair p2) (len p1)) then
                                let p2len = maxConLen (pair p2) in
                                  if p2len > len p1 then
                                    PairSet (pair p2) p2len
                                  else p1
                             else p1

quadratic :: Pair -> Int -> Int
quadratic (Pair a b) x = x ^ 2 + a * x + b

maxConLen :: Pair -> Int
maxConLen p = length $ takeWhile isPrime ( map f [0..] )
  where f = quadratic p

primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x<-xs, (x< p*p) || (x `mod` p /= 0)]

isPrime :: Int -> Bool
isPrime n
  | abs n < 2 = False
  | otherwise   = null [ x | x <- squPrimes , pn `mod` x  == 0]
        where pn = abs n
              square = fromIntegral $ round $ sqrt $ fromIntegral pn
              squPrimes = takeWhile (<= square) primes
