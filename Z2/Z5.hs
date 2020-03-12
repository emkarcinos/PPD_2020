--a)

evenElementsCount :: [Integer] -> Integer
evenElementsCount [] = 0
evenElementsCount list = 
    if ((mod (head list) 2) == 0)
        then 1 + evenElementsCount (tail list)
    else 0 + evenElementsCount (tail list)

--b)

byThreeCount :: [Integer] -> Integer
byThreeCount [] = 0
byThreeCount list =
    if ((mod (head list) 3) == 0)
        then 1 + byThreeCount (tail list)
    else 0 + byThreeCount (tail list)

--c)

sumByThree :: [Integer] -> Integer
sumByThree [] = 0
sumByThree list = summing list 0
summing :: [Integer] -> Integer -> Integer
summing [] n = n
summing list n =
    if ((mod (head list) 3) == 0)
        then summing (tail list) (n + (head list))
    else summing (tail list) n

