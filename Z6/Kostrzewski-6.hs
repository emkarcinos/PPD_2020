--Z2
elemCounter :: Eq a => [a] -> [(a,Int)]
elemCounter [] = []
elemCounter (a:ax) = [(a, length (filter (==a) (a:ax)))] ++ elemCounter (filter (/= a) ax)

--Z3
data BinTree a = Empty | Node a (BinTree a) (BinTree a)

shortestBranchLen :: BinTree a -> Integer
shortestBranchLen tree = minimum (inorderCount tree 0)

inorderCount :: BinTree a -> Integer -> [Integer]
inorderCount Empty result = [result]
inorderCount (Node _ l r) result = inorderCount l (result + 1)  ++ inorderCount r (result + 1)

--Z4
data TriTree a = TEmpty | TNode a (TriTree a) (TriTree a) (TriTree a)

sumTriTreeElements :: Num a => TriTree a -> a
sumTriTreeElements tree = sum (triTraverse tree)

triTraverse :: TriTree a -> [a]
triTraverse TEmpty = []
triTraverse (TNode a l c r) = [a] ++ triTraverse l ++ triTraverse c ++ triTraverse r

--Z5
class Kostrzewski a where
    pole :: a -> Double
    obwod :: a -> Double

instance Kostrzewski Marcin where
    obwod (Trojkat a b c) = a + b + c
    pole (Trojkat a b c) = sqrt ((obwod (Trojkat a b c)) / 2 *
                                (((obwod (Trojkat a b c)) / 2) - a) *
                                (((obwod (Trojkat a b c)) / 2) - b) *
                                (((obwod (Trojkat a b c)) / 2) - c))

data Marcin = Trojkat Double Double Double deriving Show

{-
*Main> pole (Trojkat 5.0 4.0 3.0) 
6.0
*Main> obwod (Trojkat 3.0 5.0 4.0)
12.0
-}