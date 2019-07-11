substr :: Eq a => Num a => a->a->String ->String
substr _ _ [] = []
substr 0 n (x:xs) = x:substr 0 (n-1) xs
substr p n (x:xs) = substr (p-1) n xs 