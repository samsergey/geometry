{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck -- (property, Arbitrary (..), oneof, sample)
import Test.Invariant

import Data.Complex
import Data.Fixed (mod')

import Base
import Transform
import Point
import Circle
import Geometry


main :: IO ()
main = hspec $ do
  describe "Circle" $ do
    describe "parametrization" $ do
      it "1" $ do
        property $ \c n -> not (Circle.isTrivial c) ==>
                               locus c (param c n) ~==  n `mod'` 1
      it "2" $ do
        property $ \c n -> let p = c `param` n
                           in  not (Circle.isTrivial c) ==>
                               param c (locus c p) ~==  p
      it "3" $ do
        property $ \c n k -> not (Circle.isTrivial c) ==>
                           c `param` n ~== c `param` (n + fromIntegral (k :: Int))

    describe "location" $ do
      it "1" $ do
        property $ \c -> not (Circle.isTrivial c) ==>
                         location (center c) c == Inside
      it "2" $ do
        property $ \c n -> location @Circle (c `param` n) c == OnCurve

      it "3" $ do
        property $ \c n -> let Vec nr = normal c n
                           in not (Circle.isTrivial c) ==>
                              location @Circle (pos (c `param` n) + nr) c == Outside

    describe "tangent" $ do
      it "1" $ do
        property $ \c n -> not (Circle.isTrivial c) ==>
                           normal c n `perp` tangent c n
      it "2" $ do
        property $ \c n -> not (Circle.isTrivial c) ==>
                           signum (normal c n `cross` tangent c n) == orientation c

      it "3" $ do
        property $ \c n -> not (Circle.isTrivial c) ==>
                           isZero (normal c n + normal c (n + 1/2))

