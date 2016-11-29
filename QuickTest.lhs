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

exercise 5.4
============

exercise 5.5
============
