{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ex_5.III where

import Ex_5.HList
import Preamble hiding (All, show)
import GHC.Show


type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where 
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) = case compare a b of
    EQ         -> compare as bs
    lessOrMore -> lessOrMore

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (x :# xs) = show x <> " :# " <> show xs
