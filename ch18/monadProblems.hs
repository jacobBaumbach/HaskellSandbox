module MonadProblems where

import Control.Monad (join, liftM2)

j :: Monad m => m ( m a) -> m a
j a = join a

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = fmap f m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = liftM2 f a b

a :: Monad m => m a -> m (a -> b) -> m b
a m mf = mf <*> m

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh a f = go a f (return []) where
     go [] _ acc = acc
     go (a:b) f' acc = go b f' ((f' a) >>= (\x -> acc >>= (\y -> return $ y++[x])))

flipType :: (Monad m) => [m a] -> m [a]
flipType lm = meh lm id