> {-# LANGUAGE UnicodeSyntax #-}
> module QuickTest (Probes, Property, (-->), (==>))
> where
> import Unicode
> import Data.List (sort)

> type Probes a    =  [a]
>
> type Property a  =  a → Bool

> infixr 1  -->, ==>
>
> (-->)   ∷ Probes a → Property b → Property (a → b)
> (==>)   ∷ Probes a → (a → Property b) → Property (a → b)
>
> probes --> prop  =  \ f → and [ prop (f x) | x ← probes ]
> probes ==> prop  =  \ f → and [ prop x (f x) | x ← probes ]

ordered      ∷ (Ord a) ⇒ Property [a]
permutations ∷ [a] → Probes [a]

> isqrt ∷ Integer → Integer
> isqrt n = loop 0 3 1
>   where loop i k s  | s ≤ n      = loop (i + 1) (k + 2) (s + k)
>                     | otherwise  = i

infixr 4  ⊗
(⊗) ∷ Probes a → Probes b → Probes (a, b)

> niftySort ∷ [a] → [a]
> niftySort _xs  =  []
>
> trustedSort ∷ (Ord a) ⇒ [a] → [a]
> trustedSort  =  sort

--------------------------------------------------------------------------------

author: Hendrik Werner s4549775

exercise 5.1
============

> ordered :: (Ord a) => Property [a]
> ordered [] = True
> ordered [a] = True
> ordered (a:b:as)
>     | a <= b = ordered (b:as)
>     | otherwise = False

exercise 5.2
============

> permutations :: [a] -> Probes [a]
> permutations [] = [[]]
> permutations (a:as) = [x | p <- permutations as, x <- insertEveryWhere a p]

> insertEveryWhere :: a -> [a] -> [[a]]
> insertEveryWhere a [] = [[a]]
> insertEveryWhere a (b:bs) = (a:b:bs):[b:x | x <- insertEveryWhere a bs]

There are n! permutations of a list with n entries.

exercise 5.3
============

Here I inserted the code for convenience.

> runs :: (Ord a) => [a] -> [[a]]
> runs [] = []
> runs (a:as) = addToRun a $ runs as

> addToRun :: (Ord a) => a -> [[a]] -> [[a]]
> addToRun a [] = [[a]]
> addToRun a ((c:cs):rs)
>     | a <= c = (a:c:cs):rs
>     | otherwise = [a]:(c:cs):rs

Check that the runs contains ordered content.

(permutations ['a'..'d'] --> and . map ordered) runs

Check that the runs themselfs are in the right order.

(permutations ['a'..'d'] ==> \inp res -> concat res == inp) runs

exercise 5.4
============

> isIntegerSqrt :: Property (Integer -> Integer)
> isIntegerSqrt f = ([0..100] ==> (\inp res -> res == integerSqrt inp)) f

> integerSqrt :: Integer -> Integer
> integerSqrt i = toInteger $ fromIntegral $ floor $ sqrt $ fromIntegral i

The following expression can be used to test if isqrt is a proper integerSqrt
function.

isIntegerSqrt isqrt

Of course just by testing it is impossible to establish the correctness of the
function because Integer is an unbounded data type and we cannot exhaustively
try all possible values. We can however establish that a function is NOT a
proper integerSqrt function.

Is a functions fails this test is is definitely wrong. If it passes the test it
is probably correct.

exercise 5.5
============
