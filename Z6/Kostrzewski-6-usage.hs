import Kostrzewski6Mojzbior

-- Suma trzech zbiorów
sum3 :: Eq a => [a] -> [a] -> [a] -> [a]
sum3 a b c = suma (suma a b) c

{-
*Main> sum3 [1] [1,2,5,3] [2, 5, 8]
[1,2,5,3,8]
-}

-- Iloczyn trzech zbiorów
mul3 :: Eq a => [a] -> [a] -> [a] -> [a]
mul3 a b c = iloczyn (iloczyn a b) c

{-
*Main> mul3 [1,5] [1,2,5,3] [2, 5, 8]
[5]
-}

-- Usuwanie pierwszego elementu z listy za pomocą różnicy zbiorów
del1 :: Eq a => [a] -> [a]
del1 (a:ax) = roznica ax [a]

{-
*Main> del1 [2,3,4,5]
[3,4,5]
-}