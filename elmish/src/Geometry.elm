module Geometry exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Monoid exposing (..)
import Commons exposing (..)
import Transform exposing (..)
import Circle exposing (Circle (..))
import Point exposing (Point (..))
import Line exposing (Line (..))

------------------------------------------------------------
type Figure = P Point
            | C Circle
            | L Line
            | Group (List Figure)

------------------------------------------------------------

point p = P <| Point.constructor p
pointOn c t = point <| getPoint c t

circle c x = C <| Circle.constructor c x

line p1 p2 = L <| Line.constructor p1 p2

------------------------------------------------------------
            
iso getter fig =
  case fig of
    C (Circle p) -> getter p.curve
    L (Line l) -> getter l.curve
    _ -> getter trivialCurve

getPoint = iso (.point)
getLocus = iso (.locus)
getTangent = iso (.tangent)


             
refPos fig =
  case fig of
    P p -> Point.refPos p
    C c -> Circle.refPos c
    L l -> Line.refPos l
    Group [] -> (0, 0)
    Group (xs :: _) -> refPos xs

------------------------------------------------------------

transform t fig =
  case fig of
    P p -> P <| Point.transform t p
    C c -> C <| Circle.transform t c
    L l -> L <| Line.transform t l
    Group fs -> Group (List.map (transform t) fs)

transformAt t p = 
  let (x, y) = refPos p
  in translate (-x, -y) >> t >> translate (x, y)
                      
translate = transform << translateT
scale a b = transform (scaleT a b)
scaleAt a b = transformAt (scale a b)
rotate a = transform (rotateT a)
rotateAt a = transformAt (rotate a)
vector p1 p2 = sub (refPos p2) (refPos p1)
superpose p1 p2 = translate (vector p1 p2)
at p1 p2 = superpose p1 p2 p1

------------------------------------------------------------

type alias Box = ((Float,Float),(Float,Float))
mbox = mtuple (mtuple mmin mmin) (mtuple mmax mmax)

box fig =
    case fig of
        P (Point p) -> (p.xy, p.xy)
        C (Circle c) -> let (x, y) = c.center
                        in ((x-c.r, y-c.r), (x+c.r, y+c.r))
        L (Line l) -> (l.pivot, l.pivot)
        Group fs -> foldMap mbox box fs

figureWidth f = let ((x1, _), (x2, _)) = box f
         in x2 - x1

figureHeight f = let ((_, y1), (_, y2)) = box f
           in y2 - y1

------------------------------------------------------------

show chart fig =
  case fig of
    P p -> Point.show chart p
    C c -> Circle.show chart c
    L l -> Line.show chart l
    Group fs -> (List.concatMap (show chart) fs)
    
------------------------------------------------------------
scaling (a,b) (c,d) x = c + (x - a)/(b - a)*(d - c)
  
mkChart name =
  let scaleX = scaling (-20, 20) (0, 400)
      scaleY = scaling (-20, 20) (400, 0)
  in { put = svg [ id name
                 , width "400"
                 , height "400"
                 , viewBox "0 0 400 400"
                 , stroke "orange"
                 , strokeWidth "2"
                 , fill "none" ] 
     , scale = { x = scaleX, y = scaleY }
     , xy = \(x, y) ->
            String.concat [ String.fromFloat (scaleX x)
                          , ","
                          , String.fromFloat (scaleY y)
                          , " "]
     }
