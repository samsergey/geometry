module FiguresSpec where
  
import Test.Hspec
import Geometry.DocFigs

--spec :: Spec
spec = it "rendering" $ docFigs `shouldReturn` () 
