length2 :: Num b => [a] -> b
length2 [x] = foldr(\_ acc -> 1+acc) 0 [x]

or2 :: [Bool] -> Bool
or2 [x] = foldr(||) False [x]

elem2 :: Eq a => a -> [a] -> Bool
elem2 a [b] = foldr(\y acc -> acc || y == a) False [b]

map2 :: (t -> b) -> [t] -> [b]
map2 f [x] = foldr(\x acc -> acc ++ [f x]) [] [x]
