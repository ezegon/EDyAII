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
--      d)     --
-----------------

insert :: Ord k => (k, a) -> BTree32 k a -> BTree32 k a
insert n Nil = node Nil n Nil
insert (key, new_data) (l _ (k, a) r) | key == k = node l (key, new_data) r
                                    | key < k  = balance (insert (key, new_data) l)
                                    | key > k  = balance (insert (key, new_data) r)

-----------------
--      e)     --
-----------------

delRoot :: Ord k => BTree32 k a -> BTree32 k a
delRoot Nil = Nil
delRoot (Node Nil _ Nil) = Nil
delRoot (Node _ l n r) | r == Nil  = l
                       | l == Nil  = r
                       | size l < size r = let t = replaceR r in node l (fst t) (snd t)
                       | size r < size l = let t = replaceL l in node (snd t) (fst t) r
                       | otherwise = let t = replaceR r in node l (fst t) (snd t)

replaceR :: Ord k => BTree32 k a -> Btree32 k a
replaceR (Node _ Nil n Nil) = (n, Nil)
replaceR (Node _ l n r)) | r != Nil = let t = replaceR r in (fst t, node l n (balance (snd t))) 
                         | r == Nil = (n, Nil)

replaceL :: Ord k => BTree32 k a -> Btree32 k a
replaceL (Node _ Nil n Nil) = (n, Nil)
replaceL (Node _ l n r)) | l != Nil = let t = replaceL l in (fst t, node (balance (snd t)) n r) 
                         | l == Nil = (n, Nil)

-----------------
--     AUX     --
-----------------

node :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
node Nil n Nil = Node Nil 1 n Nil
node Nil n r@(Node _ sr _ _) = Node Nil (sr+1) n r
node l@(Node _ sl _ _) n Nil = Node l (sl+1) n Nil
node l@(Node _ sl _ _) n r@(Node _ sr _ _) = Node l (sl + sr + 1) n r
