> {-# LANGUAGE UnicodeSyntax #-}
> module WordList
> where
> import Prelude hiding (Word)
> import Unicode
> import Data.Char
> import Data.List

> type Word  =  String

> lorem ∷ String
> lorem
>   = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
>     \nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam \
>     \erat, sed diam voluptua. At vero eos et accusam et justo duo dolores \
>     \et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
>     \Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur \
>     \sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et \
>     \dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam \
>     \et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea \
>     \takimata sanctus est Lorem ipsum dolor sit amet."

wordList ∷ String → [(Word, Int)]

--------------------------------------------------------------------------------

author: Hendrik Werner s4549775
author: Jasper Haasdijk s4449754

exercise 1
==========

> wordList :: String -> [(Word, Int)]
> wordList s = sortOn (\(a, b) -> b) $ map (\x -> (head x, length x)) (groups s)

> groups :: String -> [[String]]
> groups s = group $ sort $ words $ sanitize s

> sanitize :: String -> String
> sanitize s = filter (\x -> isLetter x || isSpace x) $ map toLower s

> formatRuns :: (Show a) => [a] -> String
> formatRuns as = intercalate "\n" $ map show as



When using "putStr $ formatRuns $ wordList lorem" the output is shown one entry
per line.