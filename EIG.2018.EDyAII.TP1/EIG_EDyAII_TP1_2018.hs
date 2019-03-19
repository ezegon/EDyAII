module TP1 where

import Data.List

data NdTree p = Node (NdTree p)
                     p
                     (NdTree p)
                     Int
                     | Empty
    deriving (Eq, Ord, Show)
    
class Punto p where
    dimension :: p -> Int
    coord :: Int -> p -> Double
    dist :: p -> p -> Double
    dist p q = sum [s^2 | s <- [(coord i q - coord i p) | i <- [0..(dimension p - 1)]]]

newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

instance Punto Punto2d where
    dimension _ = 2
    coord n p@(P2d t) | n-1 > dimension p = error "se supera la dimension"
                      | n == 0 = fst t
                      | n == 1 = snd t
    
instance Punto Punto3d where
    dimension _ = 3
    coord n p@(P3d (a,b,c)) | n-1 > dimension p = error "se supera la dimension"
                            | n == 0 = a
                            | n == 1 = b
                            | n == 2 = c
              
instance Eq Punto2d where
    P2d (a,b) == P2d (c,d) = (a,b) == (c,d)
    
instance Show Punto2d where
    show (P2d (x,y)) = "(" ++ show x ++ "," ++ show y ++ ")"

{-
For testing purposes

right :: Punto p => NdTree p -> NdTree p
right Empty = Empty
right (Node _ _ r _) = r


left :: Punto p => NdTree p -> NdTree p
left Empty = Empty
left (Node l _ _ _) = l
-}

getNode :: Punto p => NdTree p -> p
getNode (Node _ n _ _) = n


median :: Punto p => [p] -> Int -> (p, Double)
median xs e = (xs !! ((length xs) `div` 2), coord e (xs !! ((length xs) `div` 2)))

              
magicTree :: Punto p => [p] -> Int -> NdTree p
magicTree [] _ = Empty
magicTree [x] level = Node Empty x Empty e where e = level`mod`(dimension x)
magicTree p@(x:xs) level = Node l mp r e where e = level`mod`(dimension x)
                                               sl = sortOn (coord e) p
                                               mr = median sl e
                                               m = snd mr
                                               mp = fst mr
                                               mi = (length p) `div`2
                                               l = magicTree [q | (q, i) <- zip sl [0..], coord e q <= m, i/=mi] (level+1)
                                               r = magicTree [q | (q, i) <- zip sl [0..], coord e q > m, i/=mi] (level+1)

              
fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList [x] = Node Empty x Empty 0
fromList p@(x:xs) = magicTree p 0


magicInsert :: Punto p => p -> NdTree p -> Int -> NdTree p
magicInsert x Empty level = Node Empty x Empty (level`mod`(dimension x))
magicInsert x (Node Empty n Empty e) level | coord e x <= coord e n = Node (Node Empty x Empty ((level+1)`mod`(dimension x))) n Empty e
                                           | otherwise = Node Empty n (Node Empty x Empty ((level+1)`mod`(dimension x))) e
magicInsert x (Node l n r e) level | coord e x <= coord e n = Node (magicInsert x l ((level+1)`mod`(dimension x))) n r e
                                   | otherwise = Node l n (magicInsert x r ((level+1)`mod`(dimension x))) e


insertar :: Punto p => p -> NdTree p -> NdTree p
insertar x Empty = Node Empty x Empty 0
insertar x (Node Empty n Empty e) | coord e x <= coord e n = Node (Node Empty x Empty (1`mod`(dimension x))) n Empty e
                                  | otherwise = Node Empty n (Node Empty x Empty (1`mod`(dimension x))) e
insertar x t = magicInsert x t 0


maxCoord :: Punto p => Int -> p -> p -> p
maxCoord e p q | coord e p >= coord e q = p
               | otherwise = q


minCoord :: Punto p => Int -> p -> p -> p
minCoord e p q | coord e p <= coord e q = p
               | otherwise = q


minCoordInTree :: (Eq p, Punto p) => p -> Int -> NdTree p -> p
minCoordInTree x e Empty = x
minCoordInTree x e (Node Empty n Empty _) | coord e n <= coord e x = n
                                          | otherwise = x
minCoordInTree x e (Node l n r _) | coord e n < coord e x = minCoord e n (minCoord e (minCoordInTree x e l) (minCoordInTree x e r))
                                  | otherwise = minCoord e x (minCoord e (minCoordInTree x e l) (minCoordInTree x e r))


maxCoordInTree :: (Eq p, Punto p) => p -> Int -> NdTree p -> p
maxCoordInTree x e Empty = x
maxCoordInTree x e (Node Empty n Empty _) | coord e n > coord e x = n
                                          | otherwise = x
maxCoordInTree x e (Node l n r _) | coord e n > coord e x = maxCoord e n (maxCoord e (maxCoordInTree n e l) (maxCoordInTree n e r))
                                  | otherwise = maxCoord e x (maxCoord e (maxCoordInTree x e l) (maxCoordInTree x e r))
                                  

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar x Empty = Empty
eliminar x t@(Node Empty n Empty _) | x == n = Empty
                                    | otherwise = t
eliminar x t@(Node l n r e) | x /= n && coord e x <= coord e n = Node (eliminar x l) n r e
                            | x /= n && coord e x > coord e n = Node l n (eliminar x r) e
                            | x == n && r /= Empty = let min = minCoordInTree (getNode r) e r in (Node l min (eliminar min r) e)
                            | otherwise = let max = maxCoordInTree (getNode l) e l in (Node (eliminar max l) max r e)
                           
                            
type Rect = (Punto2d, Punto2d)


isInRect :: Punto2d -> Rect -> Bool
isInRect p (x,y) = coord 0 (minCoord 0 x y) <= coord 0 p && coord 0 (maxCoord 0 x y) >= coord 0 p && coord 1 (minCoord 1 x y) <= coord 1 p && coord 1 (maxCoord 1 x y) >= coord 1 p


ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch _ (P2d (x,y), P2d (a,b)) | x == a || y == b = []
ortogonalSearch Empty _ = []
ortogonalSearch (Node Empty n Empty _) rect | isInRect n rect = [n]
                                            | otherwise = []
ortogonalSearch t@(Node l n r e) rect@(x,y)
    | isInRect n rect = n : (ortogonalSearch l rect ++ ortogonalSearch r rect)
    | coord e n <= coord e (maxCoord e x y) && coord e n >= coord e (minCoord e x y) = ortogonalSearch l rect ++ ortogonalSearch r rect
    | coord e n >= coord e (minCoord e x y) = ortogonalSearch l rect
    | coord e n <= coord e (maxCoord e x y) = ortogonalSearch r rect
