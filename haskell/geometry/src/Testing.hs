module Testing where
import Generals
import Geometry
import Test.QuickCheck
import Data.Complex

instance Arbitrary Directed where
  arbitrary = oneof [Vec <$> arbitrary, Ang <$> arbitrary]

testDirected = do
  quickCheck ((\a n -> a == a + fromIntegral n*360) :: Directed -> Int -> Bool)
  quickCheck ((\a -> (toAng . toVec . Ang) a == Ang a) :: Number -> Bool)
  quickCheck ((\a -> (toVec . toAng . Vec) a == Vec a) :: XY -> Bool)
  quickCheck ((\a -> toTurns a >= 0 && toTurns a <= 1) :: Directed -> Bool)

