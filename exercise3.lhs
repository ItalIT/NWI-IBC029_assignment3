author: Hendrik Werner s4549775

exercise 3
==========

> runs :: (Ord a) => [a] -> [[a]]
> runs [] = []
> runs (a:as) = addToRun a $ runs as

> addToRun :: (Ord a) => a -> [[a]] -> [[a]]
> addToRun a [] = [[a]]
> addToRun a ((c:cs):rs)
>     | a <= c = (a:c:cs):rs
>     | otherwise = [a]:(c:cs):rs

Partitioning a list into runs is especially useful for mergesort but can also be
used by other sorting algorithms. For mergesort you can make use of the natural
ordering in the input data to reduce the number of costly merges. For other
sorting methods they can sort the runs just by looking at the first and last
element of each runs also reducing the number of comparisons.
