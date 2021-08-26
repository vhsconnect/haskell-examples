-- Is similar to factorial of a number, In primorial, not all the natural numbers get multiplied, only prime numbers are multiplied to calculate the primorial of a number. It's denoted with P# and it is the product of the first n prime numbers.
--
--
-- given n calculate its primorial
--
--
primes = filter isPrime [2 ..]
  where
    isPrime :: Int -> Bool
    isPrime x = 2 == (length $ filter (== 0) $ map (\y -> mod x y) [1 .. x])

premorial :: Int -> Integer
premorial x = toInteger $ foldl (*) 1 $ take x primes

-------------------------------------------------------------------------
--
numPrimorial :: Int -> Integer
numPrimorial n = product $ take n $ prime [2 ..]
  where
    prime (p:ps) = p : prime [x | x <- ps, mod x p /= 0]

-------------------------------
numPrimorial :: Int -> Integer
numPrimorial = product . nPrimes

nPrimes n = take n [x | x <- [2 ..], isPrime x]

isPrime 1 = False
isPrime n = isPrime' n (n - 1)

isPrime' _ 1 = True
isPrime' n t = (n `mod` t /= 0) && isPrime' n (t - 1)

------------------
numPrimorial :: Int -> Integer
numPrimorial n = foldr (*) 1 $ take n primes
  where
    primes = sieve [2 ..]
      where
        sieve (p:t) = [p] ++ sieve [n | n <- t, rem n p > 0]

----------------
isPrime n = length [x | x <- [1 .. n], n `mod` x == 0] == 2

numPrimorial :: Int -> Integer
numPrimorial n = product . take n . filter isPrime $ [2 ..]

----------------
divides n d = n `mod` d == 0

isPrime n = not $ any (divides n) [2..round $ sqrt (fromIntegral n)]

numPrimorial n = product $ take n $ filter isPrime [2..]

------------------


primes :: [Integer]
primes = 2 : foo [3,5..]
  where foo (x:xs) = x : foo (filter ((/= 0) . flip mod x) xs)

numPrimorial :: Int -> Integer
numPrimorial n = product $ take n primes 

