module ArrSeq where

import Seq
import Par
import qualified Arr as A

{-
    Ezequiel Ivan Gonzalez
    G-5049/1
    LCC 2018
-}

instance Seq A.Arr where
    --Generate empty sequence as empty array
    emptyS = (A.empty)

    --Generate single element sequence as single element array
    singletonS x = tabulateS (\_ -> x) 1

    --Returns length of sequence
    lengthS = (A.length)

    --Returns n-th element of the sequence
    nthS = (A.!)

    --Generates a sequence based on function
    tabulateS = A.tabulate

    --Applies a function to each element of the sequence
    mapS f a = tabulateS (f . nthS a) (lengthS a)

    --Returns the longest subsequence with the elements that makes true the condition
    filterS f a = A.flatten (mapS p a)
        where p item | f item = singletonS item
                     | otherwise = emptyS

    --Concats to sequences
    appendS x y = A.flatten (tabulateS (\i -> if i==0 then x else y) 2)

    --Returns the subsequence with the first n elements
    takeS x n = A.subArray 0 n x

    --Returns the subsequence without the first n elements
    dropS x n = A.subArray n (lengthS x - n) x

    --Shows the sequence as a tree
    showtS x = case lengthS x of
               0 -> EMPTY
               1 -> ELT (nthS x 0)
               _ -> NODE (takeS x m) (dropS x m)
        where m = div (lengthS x) 2

    --Shows the sequence as a list
    showlS x = case lengthS x of
               0 -> NIL
               _ -> CONS (nthS x 0) (dropS x 1)

    --Flattens a sequence of sequences
    joinS = A.flatten

    --Equivalent of fold
    reduceS f z x | lengthS x == 0 = z
                  | otherwise = f z (reduceHelper f z x)
        where reduceHelper f z x | lengthS x == 1 = nthS x 0
                                 | otherwise = reduceHelper f z (contract f x)

    --Prefix sum
    scanS f e x = case lengthS x of
                  0 -> (emptyS, e)
                  1 -> (singletonS e, f e (nthS x 0))
                  _ -> expand f x (scanS f e (contract f x))

    --Generates a sequence from a list
    fromList = A.fromList

{-
    Extra functions
-}

contract f x | l == 1 = x
             | otherwise = tabulateS g (half l)
    where l = lengthS x
          g i | j == l-1 = nthS x j
              | otherwise = f (nthS x j) (nthS x (j+1))
              where j = 2*i

half n = q+m where (q,m) = divMod n 2

expand f a (b,x) = (tabulateS g (min (2*lengthS b) (lengthS a)), x)
    where g i | even i = y
              | otherwise = f y (nthS a (i-1))
              where y = nthS b (div i 2)
