> {-# LANGUAGE UnicodeSyntax #-}
> module DNA
> where
> import Prelude hiding (filter)
> import Unicode
> import List

Nucleobases or DNA-bases are the basic building blocks of
deoxyribonucleic acid (DNA).

> data Base  =  A | C | G | T
>   deriving (Eq, Ord)

Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

> instance Show Base where
>   showsPrec _ A  =  showChar 'A'
>   showsPrec _ C  =  showChar 'C'
>   showsPrec _ G  =  showChar 'G'
>   showsPrec _ T  =  showChar 'T'
>
>   showList  =  foldr (.) id . map shows

> base ∷ Char → Maybe Base
> base 'A'  =  Just A
> base 'C'  =  Just C
> base 'G'  =  Just G
> base 'T'  =  Just T
> base _    =  Nothing

> type DNA      =  [Base]
> type Segment  =  [Base]

> dna  ∷  DNA
> dna  =  [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

> mm  ∷  DNA
> mm  =  filter base
>    "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
>    \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
>    \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
>    \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
>    \GACAATTTAATAT\
>    \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
>    \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
>    \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
>    \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

> readDNA ∷ FilePath -> IO [Base]
> readDNA path
>   =  do  x ← readFile path
>          return (filter base x)

contains            ∷ Segment → DNA → Bool
longestOnlyAs       ∷ DNA → Integer
longestAtMostTenAs  ∷ DNA → Integer

If you want to test your code on a larger example, say within GHCi

dna <- readDNA "mm1.dna"
longestOnlyAs dna

--------------------------------------------------------------------------------

author: Hendrik Werner s4549775

exercise 4.1
============

> contains :: Segment -> DNA -> Bool
> contains s [] = False
> contains [] d = True
> contains s e@(d:ds)
>     | length s > length e = False
>     | s == take (length s) e = True
>     | otherwise = contains s ds

This version returns a list of positions of occurences of a segment in some dna
beginning at i.

> positions :: Integer -> Segment -> DNA -> [Integer]
> positions i s [] = []
> positions i s e@(d:ds)
>     | length s > length e = []
>     | take (length s) e == s = (:) i $ positions (i + 1) s ds
>     | otherwise = positions (i + 1) s ds

The same but starting at index 0, which in most cases is what you want.

> positionsFromZero :: Segment -> DNA -> [Integer]
> positionsFromZero = positions 0


exercise 4.2
============

exercise 4.3
============
