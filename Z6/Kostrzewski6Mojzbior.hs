module Kostrzewski6Mojzbior(suma, roznica, iloczyn) where   
    suma :: Eq a => [a] -> [a] -> [a]
    roznica :: Eq a => [a] -> [a] -> [a]
    iloczyn :: Eq a => [a] -> [a] -> [a]

    iloczyn [] [] = []
    iloczyn _ [] = []
    iloczyn [] _ = []
    iloczyn (a:as) (b:bs) = [a | elem a (b:bs)] ++ iloczyn as (b:bs)

    suma a b = unique (a ++ b)

    roznica [] _ = []
    roznica (a:as) b = [a | not (elem a b)] ++  roznica as b

    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique list = unique_tmp list []

    unique_tmp :: Eq a => [a] -> [a] -> [a]
    unique_tmp [] b = b
    unique_tmp (a:as) help =
        if (elem a help) 
            then unique_tmp as help
        else
            unique_tmp as (help ++ [a])