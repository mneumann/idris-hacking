-- A Binary Tree
data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                        LT => Node (insert x left) val right
                                        EQ => orig
                                        GT => Node left val (insert x right)

-- Excercise 4.1.5 (1)
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

-- Excercise 4.1.5 (2)
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ [val]  ++ (treeToList right)

-- Excercise 4.1.5 (3)
data Expr : Type where
  Num : Int -> Expr
  Add : Expr -> Expr -> Expr
  Sub : Expr -> Expr -> Expr
  Mul : Expr -> Expr -> Expr
%name Expr expr, expr1


-- Excercise 4.1.5 (4)
evaluate : Expr -> Int
evaluate (Num i) = i
evaluate (Add a b) = (evaluate a) + (evaluate b)
evaluate (Sub a b) = (evaluate a) - (evaluate b)
evaluate (Mul a b) = (evaluate a) * (evaluate b)


-- Excercise 4.1.5 (5)
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = case compare x y of
                                  LT => Just y
                                  EQ => Just y
                                  GT => Just x
