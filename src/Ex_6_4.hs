module Ex_6_4 where
  
import Preamble

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
  fmap f (Cont k) = Cont (\g -> (g . f . k) identity) 

instance Applicative Cont where
  pure a = Cont ($ a)
  Cont f <*> Cont k = Cont (\g -> (g . f identity . k) identity)

instance Monad Cont where
  Cont k >>= f = (f . k) identity

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r}
