--1)

powerlist :: [a] -> [[a]]
powerlist [] = [[]]
powerlist (a:b) = [a:c | c <- powerlist b] ++ powerlist b

--2)

iloczyn_z :: Eq a => [a] -> [a]  -> [a]
iloczyn_z [] [] = []
iloczyn_z _ [] = []
iloczyn_z [] _ = []
iloczyn_z (a:as) (b:bs) = [a | elem a (b:bs)] ++ iloczyn_z as (b:bs)

--3)

suma_z :: Eq a => [a] -> [a] -> [a]
suma_z l1 l2 = unique (l1 ++ l2)

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

--4)

roznica_z :: Eq a => [a] -> [a] -> [a]
roznica_z [] _ = []
roznica_z (a:as) b = [a | not (elem a b)] ++ roznica_z as b

--5)

{-|
    a) foldr (/) 2 [6,12,24,8]
    (6 / (12 / (24 / (8 / 2)))) => (6 / (12 / (24 / 4))) => (6 / (12 / 6)) => (6 / 2) => 3

    b) foldr (&&) True [1>2,3>2,5==5]
    (1>2 && (3>2 && (5==5 && True))) => (1>2 && (3>2 && True)) => (1>2 && True) => False

    c) foldr max 18 [3,6,12,4,55,11]
    (3 max (6 max (12 max (4 max (55 max (11 max 18)))))) => (3 max (6 max (12 max (4 max (55 max 18))))) => (3 max (6 max (12 max (4 max 55)))) => (3 max (6 max (12 max 55))) => (3 max (6 max 55)) => (3 max 55) => 55

    d) foldr (\x y -> (x+y)/2) 54 [24,4,10,6] 
    l = \x y -> (x+y)/2
    (24 l (4 l (10 l (6 l 54)))) => (24 l (4 l (10 l 30))) => (24 l (4 l 20)) => (24 l 12) => 18

    e) foldl (\x y -> (x+y)/2) 54 [2,4,10,6] 
    l = \x y -> (x+y)/2
    ((((2 l 54) l 4) l 10) l 6) => (((28 l 4) l 10) l 6) => ((16 l 10) l 6) => (13 l 6) => 9.5

    f) foldl (/) 64 [4,2,4]
    (((64 / 4) / 2) / 4) => ((16 / 2) / 4) => (8 / 4) => 2 

    g) foldl (\x y -> 2*x + y) 8 [1,2,3]
    l = \x y -> 2*x + y
    (((8 l 1) l 2) l 3) => ((17 l 2) l 3) => (36 l 3) => 75
|-}

--6)

nalezy :: Eq a => a -> [a] -> Bool
nalezy element list = 
    foldl (\x y -> x || (if y == element then True else False)) False list

--7)

mapFold :: (a -> b) -> [a] -> [b]
mapFold f l = foldl (\x y -> x ++ [(f y)]) [] l

--8)

--a)
lastFold :: [a] -> a
lastFold list = foldl1 (\x y -> y) list

--b)
headFold :: [a] -> a
headFold list = foldr1 (\x y -> x) list

--c)
maxFold :: Ord a => [a] -> a
maxFold list = foldr1 (\x y -> if (x >= y) then x else y) list