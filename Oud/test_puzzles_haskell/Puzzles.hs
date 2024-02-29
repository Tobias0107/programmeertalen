length2 :: Num b => [a] -> b
length2 = foldr(\_ acc -> 1+acc) 0

or2 :: [Bool] -> Bool
or2 = foldr (||) False

elem2 :: Eq a => a -> [a] -> Bool
elem2 a = foldr(\y acc -> acc || y == a) False

map2 :: (t -> b) -> [t] -> [b]
map2 f = foldr(\x acc -> acc ++ [f x]) []
