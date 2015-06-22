module TeraReg.QuadTree where

import Data.Monoid

-- |A tree of `Quad`s representing the decomposition of a plane.
data QuadTree a =
    Empty |
    Leaf a |
    Node a (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
    deriving Show
             
instance Functor QuadTree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a1 a2 a3 a4 a5) = Node (f a1) (fmap f a2) (fmap f a3) (fmap f a4) (fmap f a5)
    
-- |A quadrant of `Rect`s, including the bounding `Rect`.
data Quad a = Quad {
      bounds :: a,
      nw :: a,
      ne :: a,
      sw :: a,
      se :: a
    } deriving Show

instance Functor Quad where
    fmap f (Quad r qne qnw qse qsw) = Quad (f r) (f qne) (f qnw) (f qse) (f qsw)

fromRect :: Rect -> Quad Rect
fromRect r =
    Quad r (q1 r) (q2 r) (q3 r) (q4 r)

-- |A `Rect` can be divided into four quadrants
data Rect = Rect {
      left :: Int,
      top :: Int,
      width:: Int,
      height:: Int
    } deriving Show

area :: Rect -> Int
area (Rect _ _ w h) = w * h

q1 :: Rect -> Rect
q1 (Rect l t w h) = Rect l t (w `div` 2) (h `div` 2)

q2 :: Rect -> Rect
q2 (Rect l t w h) = Rect (l + (w) `div` 2) (t) (w - w `div` 2) (h `div` 2)

q3 :: Rect -> Rect
q3 (Rect l t w h) = Rect (l) (t + h `div` 2) (w `div` 2) (h - (h `div` 2))

q4 :: Rect -> Rect
q4 (Rect l t w h) = Rect (l + w `div` 2) (t + h `div` 2) (w - w `div` 2) (h - h `div` 2)

instance Monoid Rect where
    mempty = Rect 0 0 0 0
    mappend (Rect l1 t1 w1 h1) (Rect l2 t2 w2 h2) =
        Rect l t w h
            where
              l = min l1 l2
              t = min t1 t2
              w = (max (l1 + w1) (l2 + w2)) - l
              h = (max (t1 + h1) (t2 + h2)) - t

-- Create a stream of `Quad`s tiling the original `Rect`.
zorder :: Rect -> Int -> QuadTree (Quad Rect)
zorder rect marea =
    zord $ fromRect rect
    where
      zord :: Quad Rect -> QuadTree (Quad Rect)
      zord quad@(Quad r _ _ _ _) =
          if (area r) <= marea then
              Leaf quad
          else
              Node quad (zord $ fromRect $ nw quad) (zord $ fromRect $ ne quad) (zord $ fromRect $ sw quad) (zord $ fromRect $ se quad) 
