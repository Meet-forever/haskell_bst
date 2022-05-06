data BST a = 
    Nil 
    | Tree a (BST a) (BST a) deriving Show


-- Returns number of nodes in the input tree
sizeBST :: BST Int -> Int
sizeBST t = case t of
    Nil                 -> 0
    (Tree r left right) -> 1 + sizeBST left + sizeBST right


-- Returns height of input tree (height=length of longest path from root to leaf)
heightBST :: BST Int -> Int
heightBST t = case t of
    Nil                 -> -1
    (Tree r left right) -> (max (heightBST left) (heightBST right)) + 1


-- Returns smallest element in tree; returns (maxBound::Int) if input tree is empty
minBST :: BST Int -> Int
minBST t = case t of
    Nil                 -> maxBound::Int
    (Tree r left right) -> (min r (minBST left)) 

-- If tree is not BST
-- minBST t = case t of
--     Nil                 -> maxBound::Int
--     (Tree r left right) -> (min r  (min (minBST left) (minBST right)))


-- Returns largest element in tree; returns (minBound::Int) if input tree is empty
maxBST :: BST Int -> Int
maxBST t = case t of
    Nil -> minBound::Int
    (Tree r left right) -> (max r (maxBST right))

-- If tree is not BST
-- maxBST t = case t of
--     Nil                 -> minBound::Int
--     (Tree r left right) -> (max r (max (maxBST left) (maxBST right)))


-- given a number and a BST returns True if number in BST; False otherwise
memberBST :: Int -> BST Int -> Bool
memberBST i Nil = False
memberBST i (Tree v left right)
    | i == v = True
    | i < v = memberBST i left
    | i > v = memberBST i right

-- When given tree is not BST
-- memberBST i t = case t of
--     Nil -> False
--     (Tree r left right) -> (r == i) || (memberBST i left) || (memberBST i right)


-- Given a number and a BST, returns the new tree obtained by inserting number in BST
-- If number already exists in BST then return same tree
insertBST :: Int -> BST Int -> BST Int
insertBST i t =
    let insert n Nil = (Tree n Nil Nil)
        insert n (Tree m left right)
            | n < m = Tree m (insert n left) right
            | n > m = Tree m left (insert n right)
    in case t of
        Nil -> (Tree i Nil Nil)
        t   -> case () of
                _   | memberBST i t -> t
                    | otherwise -> insert i t


-- Alternative way --------------------------------------------------------
-- insertBST i t = case t of
--     Nil -> (Tree i Nil Nil)
--     otherwise -> if (memberBST i t) 
--                 then t
--                 else insert i t

-- insert :: Int -> BST Int -> BST Int
-- insert i Nil = (Tree i Nil Nil)
-- insert i (Tree j left right)
--     | i < j = Tree j (insert i left) right
--     | i > j = Tree j left (insert i right)
---------------------------------------------------------------------------


-- Given a number and a BST, returns the new tree obtained by deleting number from BST
-- If number does not exist in BST then return same tree
deleteBST :: Int -> BST Int -> BST Int
deleteBST i t =    
    let successor :: BST Int -> Int
        successor (Tree v left right) = case (left, right) of
                                        (Nil, Nil) -> v
                                        (Nil, right) -> v
                                        otherwise -> successor left
        
        delete :: Int -> BST Int -> BST Int
        delete n (Tree m left right)
            | n == m = case (left, right) of
                            (Nil, right) -> right
                            (left, Nil) -> left
                            otherwise -> (Tree (successor right) left (delete (successor right) right) ) -- On the left child find the successor and delete it
            | n < m = Tree m (delete n left) right
            | n > m = Tree m left (delete n right)
    
    in case t of
        Nil -> t
        t   -> case () of
                _   | not (memberBST i t) -> t
                    | otherwise -> delete i t


toString:: BST Int -> Int -> String
toString t n = case t of
    Nil                 -> "Nil\n" 
    (Tree x left right) -> replicate n ' ' ++ show x ++ "\n" ++ 
                            toString left  (n+2) ++ 
                            toString right (n+2)