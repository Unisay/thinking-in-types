{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ex_5_3 where

import GHC.Show
import Preamble hiding (show)
import Ex_5_3.HList


instance Eq (HList '[]) where HNil == HNil = True
instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

instance Ord (HList '[]) where compare HNil HNil = EQ
instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  compare (a :# as) (b :# bs) = case compare a b of
    EQ         -> compare as bs
    lessOrMore -> lessOrMore

{-
ghci> ('a' :# True :# HNil) `compare` ('a' :# False :# HNil)
GT
ghci> ('a' :# True :# HNil) `compare` ('b' :# False :# HNil)
LT
-}

instance Show (HList '[]) where show HNil = "HNil"
instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (x :# xs) = show x <> " :# " <> show xs
