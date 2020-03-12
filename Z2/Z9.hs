duplicate :: a -> Integer -> [a]
duplicate _ 0 = []
duplicate element n =
    element:(duplicate element (n-1))