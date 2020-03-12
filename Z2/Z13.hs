hasOccurrences :: Eq a => [a] -> [a] -> Bool
hasOccurrences [] _ = True
hasOccurrences _ [] = False
hasOccurrences template list =
    (elem (head template) list) && (hasOccurrences (tail template) list)