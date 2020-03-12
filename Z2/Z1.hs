--a)
listFirst l x = x : l

--b)
listSecond l x = (head l) : ([x] ++ (tail l))

--c)
listLast l x = l ++ [x]
