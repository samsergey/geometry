module Testing where
import Generals
import Geometry
import Test.QuickCheck

instance Arbitrary Directed where
  arbitrary = Ang <$> arbitrary
