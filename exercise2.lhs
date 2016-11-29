author: Hendrik Werner s4549775

exercise 2.1
============

> allTrue :: [Bool] -> Bool
> allTrue [] = True
> allTrue (b:bs) = b && allTrue bs

exercise 2.2
============

> allFalse :: [Bool] -> Bool
> allFalse [] = True
> allFalse (b:bs) = not b && allFalse bs

This does not use the list design pattern but is more elegant in my opinion:

allFalse x = allTrue $ map not x

exercise 2.3
============

This even works for infinite lists, as the or operator short circuits.

> member :: (Eq a) => a -> [a] -> Bool
> member e [] = False
> member e (a:as) = e == a || member e as

exercise 2.4
============

Here is one version using maxBound which satisfies the type but has the
disadvantage of being arguably wrong for empty lists.

> smallest :: [Int] -> Int
> smallest [] = maxBound
> smallest (i:is) = min i $ smallest is

exercise 2.5
============

This returns a Maybe Int instead of an Int but is correct for empty lists.

> largest :: [Int] -> Maybe Int
> largest [] = Nothing
> largest (i:is) = maybeMax i $ largest is

> maybeMax :: Int -> Maybe Int -> Maybe Int
> maybeMax i Nothing = Just i
> maybeMax i (Just mi)
>     | i >= mi = Just i
>     | otherwise = Just mi
