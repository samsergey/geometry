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
        property $ \(Nontrivial c) n ->
                     let types = c :: Circle
                     in (c <@ n) @> c ~==  n `mod'` 1
      it "2" $ 
        property $ \(Nontrivial c) n ->
                     let types = c :: Circle
                         p = c <@ n
                     in c <@ (p @> c) ~==  p
      it "3" $ 
        property $ \(Nontrivial c) n (DInt k) ->
                     let types = c :: Circle
                     in c <@ n ~== c <@ (n + k)

      it "4" $ 
        property $ \(Nontrivial c) n (Positive s) ->
                     let p = c <@ n
                     in p @> c ~== scaleAt (center c) s p @> c

    describe "location" $ do
      it "1" $
        property $ \(Nontrivial c) ->
                     location (center c) c == Inside
        
      it "2" $
        property $ \c n ->
                     let types = c :: Circle
                     in location (c <@ n) c == OnCurve

      it "3" $
        property $ \(Nontrivial c) n ->
                     let types = c :: Circle
                         Cmp nr = normal c n
                     in location (cmp (c <@ n) + nr) c == Outside

    describe "tangent" $ do
      it "1" $
        property $ \(Nontrivial c) n ->
                     let types = c :: Circle
                     in normal c n `isOrthogonal` tangent c n
          
      it "2" $
        property $ \(Nontrivial c) n -> 
                     signum (normal c n `cross` tangent c n) == orientation c

      it "3" $
        property $ \(Nontrivial c) n (DInt k) ->
                     let types = c :: Circle
                     in normal c n `isOpposite` normal c (n + k + 1/2)

      it "4" $
        property $ \(Nontrivial c) n (DInt k) ->
                     let types = c :: Circle
                     in tangent c n `isOpposite` tangent c (n + k + 1/2)

