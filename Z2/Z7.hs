--a)

sqrListMap :: [Integer] -> [Integer]
sqrListMap [] = []
sqrListMap list = map pow2 list

pow2 :: Integer -> Integer
pow2 n = n^2

--b)
sqrList :: [Integer] -> [Integer]
sqrList [] = []
sqrList (a:b) = [a^2] ++ (sqrList b)