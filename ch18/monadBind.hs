module MonadBind where

import Control.Monad (join)

bind :: Monad m => m a -> (a -> m b) -> m b
bind m f = join $ fmap f m

twiceWhenEven xs = do
     x<-xs 
     if even x then [x*x, x*x] else []