isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome list =
    if (head list) == (last list)
        then if (length list) >= 2 then isPalindrome (tail (init list))
            else True
    else False