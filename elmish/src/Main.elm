module Main exposing (main)
import Commons exposing (..)
import Geometry exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

put id figs =
  let chart = mkChart id
      saving = "save('" ++ id ++ "','" ++ id ++ ".svg')"
  in div [] [ button [ onClick saving ] [ text id ] 
            , br [] []
            , chart.put (List.concatMap (show chart) figs) ]

g = Group [point (0,0)
          , circle (2,3) 4
          ]
    
main =
  div [ ]
    [ put "ch1" [ g, rotate 30 g, line origin (3,4)  ]
    ]
