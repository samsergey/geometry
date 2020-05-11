{-# LANGUAGE OverloadedStrings #-}
module SVG where

import Graphics.Svg
import Data.Complex
import qualified Data.Text.Lazy.IO as Txt
import qualified Transform as T

paperSize = 50
svgSize = 500

class SVGable a where
  toSVG :: a -> Element

svg content =
     doctype <> with (svg11_ content) [ Version_ <<- "1.1"
                                      , Width_ <<- "500"
                                      , Height_ <<- "500" ]


chart name figs = Txt.writeFile name $ (prettyText contents)
  where
    contents = svg $ foldMap (toSVG . scaler) figs
    scaler = T.translate ((svgSize/2) :+ (svgSize/2))
           . T.scale (svgSize/paperSize)

