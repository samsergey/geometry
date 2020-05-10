module Point exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Commons exposing (..)
import Transform exposing (..)

type Point = Point { xy : XY }

refPos (Point {xy}) = xy
  
transform t (Point this) = Point {this | xy = transformV t this.xy }
                        
show chart (Point {xy}) =
  let (x, y) = xy
      gx = String.fromFloat <| chart.scale.x x
      gy = String.fromFloat <| chart.scale.y y
  in [ circle [cx gx, cy gy, r "3"
              , fill "red", stroke "#444", strokeWidth "1"] [] ]

constructor (x,y) = Point { xy = (x,y) }
