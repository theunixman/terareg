module TeraReg.QuadTree where

import Data.Monoid
    
-- -- |A tree dividing a plane into `Quad`s.
-- --
-- -- A `Leaf` is a `Quad` with no children. Each `Node` is a `Quad`
-- -- bounding box for the `QuadTree`s it contains.
-- data QuadTree =
--     Empty |
--     Leaf (Quad) |
--     Node Quad QuadTree
--     deriving Show

-- instance Monoid QuadTree where
--     mempty = Empty
--     mappend Empty Empty = Empty
--     mappend Empty (Leaf q) = Leaf q
--     mappend (Leaf q1) (Leaf q2) = Leaf $ q1 <> q2
--     mappend (Node q _) (Leaf q2) = Leaf $ q <> q2
--     mappend l n = n <> l

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
zorder :: Rect -> Int -> [Quad Rect]
zorder rect marea =
    zord [] rect
    where
      zord :: [Quad Rect] -> Rect -> [Quad Rect]
      zord t r =
          if area r <= marea then
              [Quad rect (q1 rect) (q2 rect) (q3 rect) (q4 rect)]
          else
              concatMap (\q -> zord t (q r)) [q1, q2, q3, q4]
