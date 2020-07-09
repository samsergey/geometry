
module CircleSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Fixed (mod')
import Geometry
import Geometry.Testing

spec :: Spec
spec = describe "Circle" $ do
    describe "parametrization" $ do
      it "1" $ 
        property $ \(AnyCircle c) n ->
                     (c @-> n) ->@ c ~=  n `mod'` 1
      it "2" $ 
        property $ \(AnyCircle c) n ->
                     let p = c @-> n
                     in c @-> (p ->@ c) ~=  p
      it "3" $ 
        property $ \(AnyCircle c) n (DInt k) ->
                     c @-> n ~= c @-> (n + k)

      it "4" $ 
        property $ \(AnyCircle c) n (Positive s) ->
                     let p = c @-> n
                     in p ->@ c ~= scaleAt' (center c) s p ->@ c

    describe "location" $ do
      it "1" $
        property $ \(AnyCircle c) ->
                     location c (center c) == Inside
        
      it "2" $
        property $ \(AnyCircle c) n ->
                     location c (c @-> n) == OnCurve

      it "3" $
        property $ \(AnyCircle c) n ->
                     let nr = cmp $ normal c n
                     in location c (cmp (c @-> n) + nr) == Outside

    describe "tangent" $ do
      it "1" $
        property $ \(AnyCircle c) n ->
                     normal c n `isOrthogonal` tangent c n
          
      it "2" $
        property $ \(AnyCircle c) n -> 
                     signum (normal c n `cross` tangent c n) == deg (orientation c)

      it "3" $
        property $ \(AnyCircle c) n (DInt k) ->
                     normal c n `isOpposite` normal c (n + k + 1/2)

      it "4" $
        property $ \(AnyCircle c) n (DInt k) ->
                     tangent c n `isOpposite` tangent c (n + k + 1/2)
