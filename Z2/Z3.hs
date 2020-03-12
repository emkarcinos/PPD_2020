invertList [] = []
invertList l = (invertList (tail l)) ++ [(head l)]
