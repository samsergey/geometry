{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module SVG where

import Graphics.Svg
import Data.Complex
import Data.AEq
import qualified Data.Text.Internal as Internal
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Txt
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import qualified Transform as T

paperSize = 50
svgSize = 500

class SVGable a where
  toSVG :: a -> Element
  toSVG x = mempty

  fmtSVG :: a -> Internal.Text
  fmtSVG = mempty

instance SVGable Number where
  fmtSVG n = if n ~== 0 then "0" else toShortest n

instance SVGable CXY where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y
    where (x, y) = T.coord p

instance SVGable XY where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y
    where (x, y) = T.coord p

------------------------------------------------------------

svg content =
     doctype <> with (svg11_ content) [ Version_ <<- "1.1"
                                      , Width_ <<- "500"
                                      , Height_ <<- "500" ]


chart name figs = Txt.writeFile name $ (prettyText contents)
  where
    contents = svg $ foldMap (toSVG . scaler) figs
    scaler = T.translate ((svgSize/2) :+ (svgSize/2))
           . T.scale (svgSize/paperSize)

