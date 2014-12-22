comprehensions = sum [ x | x <- [1..999], x `mod` 3 /= 0, x `mod` 5 /= 0]

sumOfEvenFibsUnder4000 = sum [ x | x <- takeWhile (<= 4000) fibs, even x]
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main = mapM_ (putStrLn.show) [comprehensions
                             ,sumOfEvenFibsUnder4000
                             ]
