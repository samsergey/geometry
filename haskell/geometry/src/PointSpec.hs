{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck -- (property, Arbitrary (..), oneof, sample)
import Test.Invariant

import Data.Complex
import Data.AEq

import Base
import Transform
import Point
import Circle
import Geometry


main :: IO ()
main = hspec $ do
  describe "Point" $ do
    describe "Pos instance" $ do
      it "1" $ do property $ pos `inverts` Point 
      it "2" $ do property $ Point `inverts` pos 
      it "1" $ do property $ coord `inverts` (Point . pos)
      it "4" $ do property $ (Point . pos) `inverts` coord

    describe "pointOn" $ do
      it "1" $ do pointOn (circle 2 origin) 0 == point ((2, 0) :: XY)
      it "2" $
        do property $ \c t -> (c :: Circle) `isContaining` pointOn c t
                    
