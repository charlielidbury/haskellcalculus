

lazyPrimes :: [Integer]
lazyPrimes = 2 : 3 : calcNextPrimes (tail lazyPrimes) [5, 7 .. ]
  where
    calcNextPrimes (p:ps) candidates
      = smallerSquareP ++ calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
        where
          (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates
      

-- genCandidates 2000000000


primes :: [Integer]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0]