-----------------------------
-- González, Ezequiel Iván --
--    Verratti, Emilio     --
-----------------------------

module TP1 where

class Diccionario t where
    vacia    :: Ord k => t k v
    insertar :: Ord k => (k, v) -> t k v -> t k v
    eliminar :: Ord k => k -> t k v -> t k v
    buscar   :: Ord k => k -> t k v -> Maybe v
    
-----------------
-- Ejercicio 1 --
-----------------

data BTree32 k a =    Nil
                    | Node
                           (Btree32 k a)
                           Int
                           (k, a)
                           (BTree32 k a)

-----------------
--      a)     --
-----------------

size :: BTree32 k a -> Int
size Nil = 0
size Node _ s _ _ = s

-----------------
--      b)     --
-----------------

search :: Ord k => k -> BTree32 k a -> Maybe a
search c Nil = Nothing
search c (Node l s (k, a) r) | c == k  = Just a
                             | c < k   = search c l
                             | c > k   = search c r
                             
-----------------
--      c)     --
-----------------
balance :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
balance l n r | size r > 3 * size l    = inv1 l n r
              | size l > 3 * size r    = inv2 l n r
              | otherwise              = node l n r
              
inv1 :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
inv1 l n r@(Node rl _ nr rr) | size rl < 2 * size rr = singleL (node l n r)
                             | otherwise             = doubleL (node l n r)

inv2 :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
inv2 l@(Node ll _ nl lr) n r | size ll < 2 * size lr = singleR (node l n r)
                             | otherwise             = doubleR (node l n r)

singleL :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a

singleL l n Nil               = node l n Nil
singleL l n (Node lr _ nr rr) = node (node l n lr) nr rr

doubleL :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a

doubleL l n Nil                                 = node l n Nil
doubleL l n (Node (Node rll _ nrl rlr) _ nr rr) = node (node l n rll) nrl (node rlr nr rr) 

singleR :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a

singleR Nil n r               = node Nil n r
singleR (Node ll _ nl rl) n r = node ll nl (node rl n r)

doubleR :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a

doubleR Nil n r                                 = node Nil n r
doubleR (Node ll _ nl (Node lrl _ nlr lrr)) n r = node (node ll nl lrl) nlr (node lrr n r)


-----------------
--     AUX     --
-----------------

node :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
node Nil n Nil = Node Nil 1 n Nil
node Nil n r@(Node _ sr _ _) = Node Nil (sr+1) n r
node l@(Node _ sl _ _) n Nil = Node l (sl+1) n Nil
node l@(Node _ sl _ _) n r@(Node _ sr _ _) = Node l (sl + sr + 1) n r
