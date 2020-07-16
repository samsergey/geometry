module FigSpec where
  
import Test.Hspec
import Geometry.DocFigs

spec :: Spec
spec = describe "Figures" $ runIO docFigs
