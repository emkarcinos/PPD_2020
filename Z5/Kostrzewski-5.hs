--Z1
data Moto = Mercedes | Ferrari | Bentley | Toyota | Chevrolet
type Kraj = String

getCountryByMoto :: Moto -> Kraj
getCountryByMoto car =
    case car of
        Mercedes -> "Germany"
        Ferrari -> "Italy"
        Bentley -> "England"
        Toyota -> "Japan"
        Chevrolet -> "USA"

--Z2
data Tree a = Empty | Node a (Tree a) (Tree a)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

{-|
*Main> preorder (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty (Node 8 Empty Empty))) (Node 3 (Node 6 Empty (Node 9 Empty Empty)) (Node 7 Empty Empty)))

[1,2,4,5,8,3,6,9,7]

*Main> inorder (Node 1 
(Node 2 (Node 4 Empty Empty) (Node 5 Empty (Node 8 Empty Empty))) 
(Node 3 (Node 6 Empty (Node 9 Empty Empty)) (Node 7 Empty Empty)))
[4,2,5,8,1,6,9,3,7]

*Main> postorder (Node 1 
(Node 2 (Node 4 Empty Empty) (Node 5 Empty (Node 8 Empty Empty))) 
(Node 3 (Node 6 Empty (Node 9 Empty Empty)) (Node 7 Empty Empty)))
[4,8,5,2,9,6,7,3,1]

*Main> preorder (Node 'a' 
(Node 'b' Empty (Node 'd' (Node 'f' Empty Empty) Empty))
 (Node 'c' (Node 'e' Empty (Node 'g' Empty Empty)) Empty)) 
"abdfceg"

*Main> inorder (Node 'a' 
(Node 'b' Empty (Node 'd' (Node 'f' Empty Empty) Empty))
 (Node 'c' (Node 'e' Empty (Node 'g' Empty Empty)) Empty))   
"bfdaegc"

*Main> postorder (Node 'a' 
(Node 'b' Empty (Node 'd' (Node 'f' Empty Empty) Empty))
 (Node 'c' (Node 'e' Empty (Node 'g' Empty Empty)) Empty)) 
"fdbgeca"
|-}

--Z3
treeMember :: Eq a => a -> Tree a -> Bool
treeMember _ Empty = False
treeMember a tree = elem a (inorder tree)

treeMember2 :: Eq a => a -> Tree a -> Bool
treeMember2 _ Empty = False
treeMember2 a tree = myElem a (inorder tree)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = 
    if a == x then True 
    else myElem a xs

--Z4
subtree :: Eq a => Tree a -> Tree a -> Bool
subtree _ Empty = False
subtree tree1 (Node _ l r) =
    if inorder l == inorder tree1 || inorder r == inorder tree1
        then True
    else
        subtree tree1 l || subtree tree1 r

--Z5
breadthTraverse :: Tree a -> [a]
breadthTraverse Empty = []
breadthTraverse (Node a l r) = [a] ++ breadthTraverseHelp (Node a l r)

breadthTraverseHelp :: Tree a -> [a]
breadthTraverseHelp Empty = []
breadthTraverseHelp (Node a l r) = getNodesValues (Node a l r) ++ breadthTraverseHelp l ++ breadthTraverseHelp r

getNodesValues :: Tree a -> [a]
getNodesValues Empty = []
getNodesValues (Node _ Empty (Node r _ _)) = [r]
getNodesValues (Node _ (Node l _ _) Empty) = [l]
getNodesValues (Node _ Empty Empty) = []
getNodesValues (Node _ (Node l _ _) (Node r _ _)) = [l] ++ [r]