import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Data.Complex
import Data.AEq

import Base
import Affine
import Point
import Circle
import Geometry
import Testing

main :: IO ()
main = hspec $ do
  describe "Point" $ do
    describe "Pos instance" $ do
      it "1" $ property $ coord `inverts` Point 
      it "2" $ property $ Point `inverts` coord 
      it "3" $ property $ coord `inverts` (Point . coord)
      it "4" $ property $ (Point . coord) `inverts` coord

    describe "pointOn" $ do
      it "1" $ pointOn (circle 2 origin) 0 == point ((2, 0) :: XY)
      it "2" $
        property $ \c t -> let types = c :: Circle
                           in c `isContaining` pointOn c t
                    
