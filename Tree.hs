module Tree
( Tree (..)
, null
, singleton
, tree_exemple
, size
, height
, insert
, search
, preOrder
, inOrder
, postOrder
, map
, toList
-- , fromList
, getMin
, getMax
-- , delete
, isBalanced
, depthFirstSearch
) where

import Prelude hiding (null, map, foldl)

data Tree a = None | Node a (Tree a) (Tree a) deriving (Show, Foldable)

tree_exemple = (Node 10 (Node 5 (Node 2 None None) (Node 7 None None)) (Node 12 None (Node 15 (Node 13 None None) (Node 17 None None))))

null :: Tree a -> Bool
null None = True
null _ = False

singleton :: a -> Tree a
singleton e = (Node e None None)


--Cette fonction prend un arbre binaire de recherche de type Tree a, et retourne le nombre de nœuds dans l'arbre.
size :: Tree a -> Int
size None = 0
size (Node _ l r) = 1 + (size l) + (size r)

height :: Tree a -> Int
height None = 0
height (Node _ l r) = 1 + max (size l) (size r)

--Cette fonction prend une valeur de type a et un arbre binaire de recherche de type Tree a
--et retourne un nouvel arbre binaire de recherche qui inclut la valeur insérée.
--Si la valeur est déjà présente dans l'arbre, la fonction ne fait rien.
insert :: Ord a => a -> Tree a -> Tree a
insert x None = (Node x None None)
insert x (Node e l r) = if e < x then
                          Node e (insert x l) r
                        else
                          Node e l (insert x r)

--Cette fonction prend une valeur de type a et un arbre binaire de recherche de type Tree a
--et retourne True si la valeur est présente dans l'arbre, ou False sinon.
search :: Ord a => a -> Tree a -> Bool
search x None = False
search x (Node e l r)
  | x == e = True
  | x < e = search x l
  | x > e = search x r

-- successor :: Ord a => a -> Tree a -> Maybe a
-- successor None = Nothing



--Cette fonction prend une valeur de type a et un arbre binaire de recherche de type Tree a
--et retourne un nouvel arbre binaire de recherche qui exclut la valeur supprimée.
--Si la valeur n'est pas présente dans l'arbre, la fonction ne fait rien.
-- delete :: Ord a => a -> Tree a -> Tree a
-- delete e None = None
-- delete e tree@(Node x l r)
--   | e < x = delete e l
--   | e > x = delete e r
--   | otherwise = case tree of (Node _ None None) -> None
--                              (Node _ l None) -> r
--                              (Node _ None r) -> l
--                              (Node _ l r) -> (Node (getMax l) (delete (getMax l)) r)
--                                 where successor = getMax l

--Cette fonction prend un arbre binaire de recherche de type Tree a
--et retourne une liste des éléments de l'arbre parcourus en ordre préfixe (racine-gauche-droit).
preOrder :: Tree a -> [a]
preOrder None = []
preOrder (Node x l r) = x : preOrder l ++ preOrder r


--Cette fonction prend un arbre binaire de recherche de type Tree a
--et retourne une liste des éléments de l'arbre parcourus en ordre infixe (gauche-racine-droit).
inOrder :: Tree a -> [a]
inOrder None = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r


--Cette fonction prend un arbre binaire de recherche de type Tree a
--et retourne une liste des éléments de l'arbre parcourus en ordre postfixe (gauche-droit-racine).
postOrder :: Tree a -> [a]
postOrder None = []
postOrder (Node x l r) = postOrder l ++ postOrder r ++ [x]

depthFirstSearch :: Tree a -> [a]
depthFirstSearch None = []
depthFirstSearch tree = aux [tree]
  where
    aux [] = []
    aux (None:xs) = aux xs
    aux ((Node e l r):xs) = e : aux (xs ++ [l, r])

map :: (a -> b) -> Tree a -> Tree b
map _ None = None
map f (Node e l r) = (Node (f e) (map f l) (map f r))

-- fold :: (a -> b -> b -> b) -> b -> Tree a -> b
-- fold _ acc None = acc
-- fold f acc (Node x l r) = f x (fold f acc l) (fold f acc r)

fold :: (a -> b -> b) -> b -> Tree a -> b
fold _ acc None = acc
fold f acc (Node x l r) = fold f (f x (fold f acc l)) r

toList :: Tree a -> [a]
toList = inOrder

-- fromList :: Ord a => [a] -> Tree a
-- fromList = foldl (\tree x -> insert x tree) None

getMin :: Tree a -> Maybe a
getMin None = Nothing
getMin (Node e None _) = Just e
getMin (Node _ l _) = getMin l

getMax :: Tree a -> Maybe a
getMax None = Nothing
getMax (Node e _ None) = Just e
getMax (Node _ _ r) = getMax r

isBalanced :: Tree a -> Bool
isBalanced None = True
isBalanced (Node _ l r) = (abs (height l - height r)) <= 1

-- flatten :: Tree a -> [a]
-- flatten = fold ()

--Cette fonction prend deux arbres binaires de recherche en entrée
--et renvoie un nouvel arbre qui contient toutes les valeurs de ces deux arbres.
merge :: Ord a => Tree a -> Tree a -> Tree a
merge tree1 None = tree1
merge tree1 (Node e l r) = (Node 1 (merge l ))

-- split :: Ord a => a -> Tree a -> (Tree a, Tree a)



{-
    predecessor : cette fonction prend une valeur x en entrée et renvoie la plus grande valeur stockée dans l'arbre qui est strictement inférieure à x.
    successor : cette fonction prend une valeur x en entrée et renvoie la plus petite valeur stockée dans l'arbre qui est strictement supérieure à x.
    merge : cette fonction prend deux arbres binaires de recherche en entrée et renvoie un nouvel arbre qui contient toutes les valeurs de ces deux arbres.
    split : cette fonction prend une valeur x et un arbre binaire de recherche en entrée et renvoie une paire de deux arbres : le premier contenant toutes les valeurs strictement inférieures à x et le second contenant toutes les valeurs supérieures ou égales à x.
    flatten : cette fonction prend un arbre binaire en entrée et renvoie une liste contenant toutes les valeurs de l'arbre, en ordre croissant.

Voici les signatures de ces fonctions :

    isBalanced :: Tree a -> Bool
    predecessor :: Ord a => a -> Tree a -> Maybe a
    successor :: Ord a => a -> Tree a -> Maybe a
    merge :: Ord a => Tree a -> Tree a -> Tree a
    split :: Ord a => a -> Tree a -> (Tree a, Tree a)
    Ord a => a -> Tree a -> (Tree a, Tree a)

-}