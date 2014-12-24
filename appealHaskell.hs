comprehensions = sum [ x | x <- [1..999], x `mod` 3 /= 0, x `mod` 5 /= 0]

sumOfEvenFibsUnder4000 = sum [ x | x <- takeWhile (<= 4000) fibs, even x]
  where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- PatternMatchingWithWhere
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l]
  where ((f:_), (l:_)) = (firstname, lastname)

patternMatchingWithWhere = initials "Tanaka" "Kazuki"

main = (putStrLn.show) (comprehensions
                       ,sumOfEvenFibsUnder4000
                       ,patternMatchingWithWhere
                       )
