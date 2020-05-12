{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck -- (property, Arbitrary (..), oneof, sample)
import Test.Invariant

import Data.Complex
import Data.AEq
import Data.Fixed (mod')

import Base
import Transform
import Point
import Circle
import Geometry


main :: IO ()
main = hspec $ do
  describe "Circle" $ do
    describe "homomorphisms" $ do
      it "1" $ do
        property $ \c n -> ((locus @Circle c) . (param @Circle c) $ n) ==  n `mod'` 1
                    
