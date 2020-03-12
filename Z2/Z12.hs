removeAtIndex :: [a] -> Int -> [a]
removeAtIndex [] _ = []
removeAtIndex (a:b) pos =
    if (pos+1) > (length (a:b)) then (a:b)
    else remover (a:b) pos []

remover :: [a] -> Int -> [a] -> [a]
remover [] _ _ = []
remover (a:b) number current =
    if number /= 0 then remover b (number-1) (current++[a])
    else current++b