--Z1
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip list = (itemizeFst list, itemizeSnd list)

itemizeFst :: [(a, b)] -> [a]
itemizeFst [] = []
itemizeFst (a:ax) = [fst a] ++ itemizeFst ax

itemizeSnd :: [(a, b)] -> [b]
itemizeSnd [] = []
itemizeSnd (a:ax) = [snd a] ++ itemizeSnd ax

--Z4
areEqual :: Eq a => [a] -> [a] -> Bool
areEqual [] [] = True
areEqual [] _ = False
areEqual _ [] = False
areEqual (a:ax) (b:bx) =
    if a==b then
        True && areEqual ax bx
    else
        False

--Z5
