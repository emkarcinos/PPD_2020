removeFirstOccurence :: Eq a => [a] -> a -> [a]
removeFirstOccurence [] _ = []
removeFirstOccurence (a:b) element =
    if a == element then b
    else a:(removeFirstOccurence b element)
