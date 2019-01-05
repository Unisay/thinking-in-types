module Ex_2 where

import Preamble

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True
