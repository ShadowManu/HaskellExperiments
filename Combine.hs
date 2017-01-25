import Data.Monoid

data Combine a = Combine a | Replace a deriving (Eq, Show)

instance (Monoid a) => Monoid (Combine a) where
  mempty = Replace mempty
  (Combine a) `mappend` (Combine b) = Combine $ a `mappend` b
  (Combine a) `mappend` (Replace b) = Replace b
  (Replace a) `mappend` (Combine b) = Combine b
  (Replace a) `mappend` (Replace b) = Replace b

data Configuration = Configuration { phrases :: [String], value :: Integer } deriving (Eq, Show)

instance Monoid Configuration where
  mempty = Configuration { phrases = [], value = 0 }
  mappend a b = Configuration { phrases = (phrases a) `mappend` (phrases b), value = max (value a) (value b) }