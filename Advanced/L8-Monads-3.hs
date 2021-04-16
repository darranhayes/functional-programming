module Monads3 where

import Prelude hiding (Functor, fmap, Applicative, pure, (<*>), Monad, (>>=), return, ST)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f(a -> b) -> f a -> f b

class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

-- State Monad
-- type ST s a = s -> (a, s) -- a is some expression result, s is some threaded state value (primative, record, etc.)

newtype ST s a = S(s -> (a, s))

-- Apply a state transformer ST to a state and get back a value and a possibly modified state
app :: ST s a -> s -> (a, s)
app (S st) s = st s

instance Functor (ST s) where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap f st = S(\s -> let (x, s') = app st s
                            x'      = f x
                         in (x', s'))

instance Applicative (ST s) where
    -- pure  :: a -> ST a
    pure x      = S(\s -> (x, s))
    -- (<*>) :: ST(a -> b) -> ST a -> ST b
    stf <*> stx = S(\s -> let (f, s')  = app stf s
                              (x, s'') = app stx s'
                           in (f x, s''))

instance Monad (ST s) where
    -- return :: a -> ST a
    return    = pure
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    stx >>= f = S(\s -> let (x, s') = app stx s
                         in app (f x) s') -- note: (f x) returns a state transformer of type b (ST b)

-- "The whole point of a State Transformer (ST) is to apply it to a State s and
--  get back a value and a possibly modified State s'"
-- aka, whereever there's an ST argument, it needs to be applied to a chained state argument s
--      and that is achieved using app.

-- Exercise: relabelling trees, manually threading state
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n   = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l', n')  = rlabel l n
                        (r', n'') = rlabel r n'

relabel :: Tree a -> Tree Int
relabel t = fst (rlabel t 0)

-- Exercise: relabelling trees, hiding state changes in the ST monad
newtype Counter = Ctr Int -- only used here to clarify the value is Int, and state is State Int below

fresh :: ST Counter Int
fresh = S(\(Ctr n) -> (n, Ctr(n + 1))) -- return value is n, and next value is encapsulated in the state position of the tuple

mlabel :: Tree a -> ST Counter (Tree Int)
mlabel (Leaf _)   = fresh >>= \n ->
                    return (Leaf n)
mlabel (Node l r) = mlabel l >>= \l' ->
                    mlabel r >>= \r' ->
                    return (Node l' r')

-- With GHC Monad, can use do notation as follows
-- mlabel :: Tree a -> ST (Tree Int)
-- mlabel (Leaf _)   = do n <- fresh
--                        return (Leaf n)
-- mlabel (Node l r) = do l' <- mlabel l
--                        r' <- mlabel r
--                        return (Node l' r')

relabel' :: Tree a -> Tree Int
relabel' t = fst (app (mlabel t) (Ctr 0))

main = do print $ relabel (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
          print $ relabel' (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))