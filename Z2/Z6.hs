hasEvenCount :: [Integer] -> Bool
hasEvenCount [] = False
hasEvenCount list =
    if mod (counter list 0) 2 == 0 then True else False

counter :: [Integer] -> Integer -> Integer
counter [] n = n
counter list n =
    counter (tail list) n+1