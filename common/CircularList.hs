module CircularList ( CircularList(..)
                    , shiftRight
                    , shiftLeft
                    , insert
                    , flatten
                    , dropFocus
                    , focus
                    ) where

data CircularList a = CircularList [a] a [a]
                      deriving Show

shiftRight :: CircularList a -> CircularList a
shiftRight c@(CircularList [] _ []) = c
shiftRight (CircularList l f []) = CircularList [] (last l) ((reverse $ init l) ++ [f])
shiftRight (CircularList l f (r:ight)) = CircularList (f:l) r ight

shiftLeft :: CircularList a -> CircularList a
shiftLeft (CircularList [] f r) = shiftLeft $ CircularList (reverse r) f []
shiftLeft (CircularList (l:eft) f r) = CircularList eft l (f:r)

insert :: a -> CircularList a -> CircularList a
insert x (CircularList l f r) = CircularList l x (f:r)

flatten :: CircularList a -> [a]
flatten (CircularList l f r) = (reverse l) ++ [f] ++ r

dropFocus :: CircularList a -> CircularList a
dropFocus (CircularList _ _ []) = error "cannot drop to empty"
dropFocus (CircularList l _ (r:ight)) = CircularList l r ight

focus :: CircularList a -> a
focus (CircularList _ f _) = f

