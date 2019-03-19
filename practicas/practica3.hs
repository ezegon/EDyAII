module Practica3 where

import Data.List

type Color = (Int,Int,Int)

mezclar :: Color -> Color -> Color
mezclar (r1,g1,b1) (r2,g2,b2) = ((r1+r2)`div`2,(g1+g2)`div`2,(b1+b2)`div`2)

type Linea = ([Char], Int)

vacia :: Linea
vacia = ([],0)

moverIzq :: Linea -> Linea
moverIzq (xs, 0) = (xs, 0)
moverIzq (xs, i) = (xs, i-1)

moverDer :: Linea -> Linea
moverDer (xs, i) | length xs == i = (xs, i)
                 | otherwise = (xs, i+1)
                 
moverIni :: Linea -> Linea
moverIni (xs, i) = (xs, 0)

moverFin :: Linea -> Linea
moverFin (xs, _) = (xs, length xs)

insertar :: Char -> Linea -> Linea
insertar c (xs, 0) = (c:xs, 1)
insertar c (xs, i) = (head xs : fst (insertar c (tail xs, i-1)), i+1)
                   
borrar :: Linea -> Linea
borrar (xs, 0) = (xs, 0)
borrar (xs, 1) = (tail xs, 0)
borrar (xs, i) = (head xs : fst (borrar (tail xs, i-1)), i-1)

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

isEmptyCL EmptyCL = True
isEmptyCL _ = False

isUnitCL (CUnit _) = True
isUnitCL _ False

headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL (CUnit _) = EmptyCL
tailCL (Consnoc _ xs _) = xs

reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = (CUnit x)
reverseCL (Consnoc x zs y) = Consnoc y (reverseCL zs) x

inits EmptyCL = []
inits (CUnit x) = [x]
inits (Consnoc x zs y) = x:y:(inits zs)

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval :: Aexp -> Int
eval (Num a) = a
eval (Prod a b) = (eval a)*(eval b)
eval (Div a b) = (eval a)`div`(eval b)

seval :: Aexp -> Maybe Int
seval (Num a) = Just a
seval (Div a (Num b)) | b == 0 || (seval a) == Nothing = Nothing
                      | otherwise = (Just (sval a)`div`b)
seval (Div a b) | (seval a) == Nothing || (seval b) == Nothing = Nothing
                | otherwise = Just (seval a)`div`(seval b)
seval (Prod a b) | (seval a) == Nothing || (seval b) == Nothing = Nothing
                 | otherwise = Just (seval a)*(seval b)
                 
data GenTree a = EmptyG | NodeG a [GenTree a]
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a)

g2bt' :: GenTree a -> [GenTree a] -> BinTree a
g2bt' EmptyG _ = EmptyB
g2bt' (NodeG n []) [] = NodeB EmptyB n EmptyB
g2bt' (NodeG n []) (h:hs) = NodeB EmptyB n (g2bt' h hs)
g2bt' (NodeG n (x:xs)) [] = NodeB (g2bt' x xs) n EmptyB
g2bt' (NodeG n (x:xs)) (h:hs) = NodeB (g2bt' x xs) n (g2bt' h hs)

g2bt :: GenTree a -> BinTree a
g2bt t = g2bt' t []
