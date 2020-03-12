counter :: Eq a => a -> [a] -> Integer
counter _ [] = 0
counter element (a:b) = 
    if a == element then 1 + (counter element b)
    else 0 + (counter element b)