module Vec ( Vec2(..)
           , normalized
           , magnitude
           , dot
           , manhattan
           ) where

data Vec2 a = Vec2 a a
            deriving (Eq,Ord,Show,Read)

instance Num a => Num (Vec2 a) where
    (+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
    (*) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)
    (-) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
    abs (Vec2 x y) = Vec2 (abs x) (abs y)
    signum (Vec2 x y) = Vec2 (signum x) (signum y)
    fromInteger int = Vec2 (fromIntegral int) (fromIntegral int)

instance Fractional a => Fractional (Vec2 a) where
    (/) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1/x2) (y1/y2)
    fromRational x = Vec2 (fromRational x) (fromRational x)

normalized :: Floating a => Vec2 a -> Vec2 a
normalized v@(Vec2 x y) = Vec2 (x/mag) (y/mag)
    where mag = magnitude v

magnitude :: Floating a => Vec2 a -> a
magnitude (Vec2 x y) = sqrt $ x*x + y*y

dot :: Num a => Vec2 a -> Vec2 a -> a
dot a b = manhattan $ a * b

manhattan :: Num a => Vec2 a -> a
manhattan (Vec2 x y) = x + y
