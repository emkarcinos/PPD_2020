swapTuples :: [(a, b)] -> [(b, a)]
swapTuples [] = []
swapTuples (a:b) =
    [(snd a, fst a)] ++ (swapTuples b)
