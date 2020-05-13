{-# Language TypeApplications #-}
import Test.Hspec
import Test.QuickCheck -- (property, Arbitrary (..), oneof, sample)
import Test.Invariant

import Data.Complex
import Data.Fixed (mod')

import Base
import Affine
import Point
import Circle
import Geometry
import Testing

main :: IO ()
main = hspec $
  describe "Circle" $ do
    describe "parametrization" $ do
      it "1" $ 
        property $ \c n -> not (isTrivial @Circle c) ==>
                               locus c (param c n) ~==  n `mod'` 1
      it "2" $ 
        property $ \c n -> let p = c `param` n
                           in  not (isTrivial @Circle c) ==>
                               param c (locus c p) ~==  p
      it "3" $ 
        property $ \c n k -> not (isTrivial @Circle c) ==>
                           c `param` n ~== c `param` (n + fromIntegral (k :: Int))

    describe "location" $ do
      it "1" $ 
        property $ \c -> not (isTrivial c) ==>
                         location (center c) c == Inside
      it "2" $ 
        property $ \c n -> location @Circle (c `param` n) c == OnCurve

      it "3" $ 
        property $ \c n -> let Cmp nr = normal c n
                           in not (isTrivial c) ==>
                              location @Circle (cmp (c `param` n) + nr) c == Outside

    describe "tangent" $ do
      it "1" $ 
        property $ \c n -> not (isTrivial @Circle c) ==>
                           normal c n `orthogonal` tangent c n
      it "2" $ 
        property $ \c n -> not (isTrivial c) ==>
                           signum (normal c n `cross` tangent c n) == orientation c

      it "3" $ 
        property $ \c n -> not (isTrivial @Circle c) ==>
                           isZero (normal c n + normal c (n + 1/2))
