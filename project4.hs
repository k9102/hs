data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 


insertTree :: (Ord a) => a->Tree a->Tree a
insertTree i Empty = Node i Empty Empty
insertTree i (Node ii ln rn) 
    | i > ii = Node ii ln (insertTree i rn)
    | otherwise = Node ii (insertTree i ln) rn

st = [1,3,5,7,2,4,6,8]
tree = foldr insertTree Empty st

elementTree :: (Ord a) => a->Tree a->Bool
elementTree i Empty = False
elementTree i (Node ii ln rn)
    | i<ii = elementTree i ln
    | i>ii = elementTree i rn
    | otherwise = True;