module Circle exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Commons exposing (..)
import Transform exposing (..)

type Circle = Circle (Curve { center : XY
                            , r : Float
                            , orientation : Float
                            , phase : Float })

radius (Circle this) = (this.center, this.curve.point 0)
refPos (Circle {center}) = center
                        
parameterize this =
  let c = this.center
      r = this.r
      w = this.orientation
      ph = this.phase
      crvPoint = turns >> \s -> add c (r * cos s, r * sin s)
      crvLocus xy = toTurns <| toAngle <| V (sub xy c)
      crvTang t = (-w*sin(2*pi*(w*t + ph)), w*cos(2*pi*(w*t + ph)))
  in Circle <| { this | curve = { point = crvPoint
                                , locus = crvLocus 
                                , tangent = crvTang }}

transform tr (Circle this) =
  let c = transformV tr this.center
      p = transformV tr (this.curve.point 0)
      t = transformV tr (this.curve.tangent 0)
      r = sub p c
  in parameterize { this
                    | center = c
                    , r = norm r
                    , phase = toTurns <| toAngle <| V (sub p c)
                    , orientation = signum (cross r t) }

show chart (Circle c) =
  let (x, y) = c.center
      scale = chart.scale
      gx = String.fromFloat (scale.x x)
      gy = String.fromFloat (scale.y y)
      gr = String.fromFloat (scale.x c.r - scale.x 0)
  in [ circle [cx gx, cy gy, r gr] [] ]


constructor c x = parameterize  { center = c
                                , r = x
                                , orientation = 1
                                , phase = 0
                                , curve = trivialCurve }
