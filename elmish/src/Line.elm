module Line exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Commons exposing (..)
import Transform exposing (..)

type Line = Line (Curve { pivot : XY
                        , vector : XY
                        , unit : Float})

refPos (Line {pivot}) = pivot
                            
parameterize l =
  let u = l.unit
      (x0, y0) = l.pivot
      (vx, vy) = l.vector
      crvPoint = \s -> (x0 + s*u*vx, y0 + s*u*vy)
      crvLocus xy = if isNullVector l.vector
                    then 0
                    else (dot (l.vector) (sub xy l.pivot))/u
      crvTang t = l.vector
  in Line <| { l | curve = { point = crvPoint
                           , locus = crvLocus 
                           , tangent = crvTang }}

transform t (Line l) =
  let p1 = transformV t <| l.curve.point 0
      p2 = transformV t <| l.curve.point 1
  in constructor p1 p2


show chart (Line l) =
  let p1 = l.curve.point (-100)
      p2 = l.curve.point (100)
      pts = String.concat <| List.map chart.xy [p1, p2]
  in [ polyline [ points pts ] [] ]

trivial = { pivot = (0, 0)
          , vector = (0, 0)
          , unit = 1
          , curve = trivialCurve }
    
isTrivial (Line {vector}) = isNullVector vector

constructor p1 p2 = parameterize { trivial
                                   | pivot = p1
                                   , vector = normalize (sub p2 p1)
                                   , unit = norm (sub p2 p1) }
