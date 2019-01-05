module Ex_7 where
  
import Preamble hiding (show)
import GHC.Show


data HasShow where
  HasShow :: Show t => t -> HasShow
  
-- instance Show HasShow where
--   show (HasShow s) = "HasShow " <> show s

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

instance Show HasShow where
  show = mappend "HasShow " . elimHasShow show
