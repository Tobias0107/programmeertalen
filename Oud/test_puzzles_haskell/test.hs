import Data.Text.Lazy.Read (double)
doubleMe :: Num a => a -> a
doubleMe x = x + x
doubleUS :: Num a => a -> a -> a
doubleUS x y = doubleMe x + doubleMe y
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
                            then x
                            else x*2
doubleSmallNumber' :: (Ord a, Num a) => a -> a
 m doubleSmallNumber' x = doubleSmallNumber x + 1
