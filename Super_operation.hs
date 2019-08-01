module Gambiara where

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< x = x >>= f

join :: (Monad m) => m (m a) -> m a
join x =  x >>= id


