> {-# LANGUAGE UnicodeSyntax #-}
> module Format
> where
> import Prelude hiding (Word)
> import Unicode
> import WordList (Word, lorem)

--------------------------------------------------------------------------------

author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 6
==========

> format :: Int -> [Word] -> [[Word]]
> format i [] = [[]]
> format i (w:ws) = merge i w $ format i ws

> merge :: Int -> Word -> [[Word]] -> [[Word]]
> merge i w (l:ls)
>     | (length w) + (sum $ map (\w -> length w + 1) l) <= i = (w:l):ls
>     | otherwise = [w]:(l:ls)

We use "(\w -> length w + 1)" because efter every word there is eiter a ' ' or a
'\n' we need to take into account.
