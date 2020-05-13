{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Data.Complex

import Base
import Affine
import Point
import Circle
import Line
import Geometry
import Testing

main :: IO ()
main = hspec $
  describe "Line" $ do
    describe "parametrization" $ do
      it "1" $ property $ \l x -> isNontrivial @Line l ==>
                                  locus l (param l x) ~== x
      it "2" $ property $ \l x -> let p = param @Line l x
                                  in param l (locus l p) ~== p
      it "3" $ line @CN @CN 0 1 `param` 0 == 0
      it "5" $ line @CN @CN 0 2 `param` 1 == 2
