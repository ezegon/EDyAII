module ListSeq where
import Seq
import Par

{-
    Ezequiel Ivan Gonzalez
    G-5049/1
    LCC 2018
-}

instance Seq [] where
    --Empty Sequence
    emptyS = []

    --Maps single element to sequence
    --singletonS x = (x:[])
    singletonS = (:[])

    --Lenght of the sequence
    --First converts all elements of the sequence in 1
    --Then reduceS sums all of them
    -- reduceS (+) 0 x = x and all x = 1
    -- reduceS (+) 0 s = (reduceS (+) 0 ls) + (reduceS (+) 0 rs)
    lengthS = reduceS (+) 0 . mapS (\_ -> 1)

    --Get nth element of the sequence
    nthS (x:_) 0 = x
    nthS (_:xs) n = nthS xs (n-1)

    --Generates a sequence based on the function
    tabulateS f n = xs ++ ys
        where m = div n 2
              (xs,ys) = mapS f [0 .. m-1] ||| mapS f [m .. n-1]

    --Applies a function to each element of the sequence
    mapS _ [] = []
    mapS f [x] = [f x]
    mapS f (x:y:ts) = a:(b:(mapS f ts))
        where (a,b) = f x ||| f y

    --Returns the longest subsequence with the elements that makes true the condition
    filterS _ [] = []
    filterS f [x] = if (f x) then [x] else []
    filterS f (x:y:ts) = appendS (appendS xs ys) (filterS f ts)
        where (xs,ys) = filterS f [x] ||| filterS f [y]

    --Concats to sequences
    appendS [] ys = ys
    appendS (x:xs) ys = x : (appendS xs ys)

    --Returns the subsequence with the first n elements
    takeS [] _ = []
    takeS xs 0 = []
    takeS (x:xs) n = x : takeS xs (n-1)

    --Returns the subsequence without the first n elements
    dropS [] _ = []
    dropS xs 0 = xs
    dropS (x:xs) n = dropS xs (n-1)

    --Shows the sequence as a tree
    showtS [] = EMPTY
    showtS [x] = (ELT x)
    showtS xs = (NODE ys zs)
        where (ys,zs) = takeS xs n ||| dropS xs n
              n = div (lengthS xs) 2

    --Shows the sequence as a list
    showlS [] = NIL
    showlS (x:xs) = (CONS x xs)

    --Flattens a sequence of sequences
    joinS = reduceS appendS emptyS

    --Equivalent of fold
    reduceS _ z [] = z
    reduceS f z (x:xs) = f x (reduceS f z (contract f xs))

    --Prefix sum
    scanS _ e [] = (singletonS e, e)
    scanS f e [x] = (singletonS e, f e x)
    scanS f e xs = (expand f ys xs, y)
        where (ys, y) = scanS f e (contract f xs)

    --Generates a sequence from a list
    fromList = id

{-
    Extra functions
-}
expand :: (a -> a -> a) -> [a] -> [a] -> [a]
expand _ [] _ = []
expand _ [_] [] = []
expand _ [y] [_] = [y]
expand f (y:ys) (x:_:xs) = let (z,zs) = f x y ||| expand f ys xs
                           in y:z:zs

contract :: (a -> a -> a) -> [a] -> [a]
contract f (x:y:ts) = let (a,as) = f x y ||| contract f ts
                      in (a:as)
contract f ts = ts
