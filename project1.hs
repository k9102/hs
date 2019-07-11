mymap :: (Num a) => (a->a) -> [a] -> [a]
mymap f [] = []
mymap f (x:xs) = f x:mymap f xs
