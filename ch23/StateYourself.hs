module StateYourself where

import Control.Monad (join)

newtype Moi s a = Moi {runMoi :: s -> (a,s)}

instance Functor (Moi s) where
	--fmap :: (a -> b) -> Moi s a -> Moi s b
	fmap f (Moi g) = Moi g' where
						g' s = (f a',s') where
							   (a', s') = g s

instance Applicative (Moi s) where
	pure a = Moi $ \s -> (a,s)
	(Moi f) <*> (Moi g) = Moi h where
		h s = (b a, t) where
			(b, t) = f s'
			(a, s') = g s

instance Monad (Moi s) where
	return = pure
	(Moi f) >>= g = Moi h where
		h s = runMoi (g a') s' where
			(a', s') =f s
