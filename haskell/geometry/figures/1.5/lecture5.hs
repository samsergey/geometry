{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

import Geometry
import Data.Complex

iterateN g f = zipWith g [1..] . iterate f

main = do
  let romashka =
        group [ aSegment # rotate (18 * fromIntegral i) #: label (show i) <> white
              | i <- [1..20] ]
      clock = aCircle <+> group (modularScale 24 (aCircle # rotate 90 ))
      hand h = mkTriangle @CN [0.03:+0, 0:+0.9, (-0.03):+0] #: fill "red" # rotate (-15 * h)
               <+> aCircle # scale 0.05 #: fill "red"
        
  writeSVG 450 "figures/1.5/26.svg" $ let
    seg = aSegment # scale (1/8) #: "1" <+> point (1/8,0)
    segs = aPoint <+> (group $ take 8 $ iterate (translate (1/8, 0)) seg)
    rays = aSegment <+>
           origin #: "O" <+>
           (group $ take 7 $ tail $ iterate (rotate 8) (aSegment #: white)) <+>
           aSegment # rotate 64 #: "1"
    in rays <+> segs # translate (0,-0.25)

  writeSVG 500 "figures/1.5/28.svg" $
    group [ aSegment # rotate (18 * fromIntegral i) #: label (show i)
          | i <- [1..20] ]

  writeSVG 500 "figures/1.5/30.svg" $
    (group [ aSegment # rotate (18 * fromIntegral i) #: label (show i) <> white
           | i <- [1..6] ]) <+>
    aSegment # rotate (18 * 7) #: "7" <+>
    aSegment <+>
    aCircle #: invisible

  writeSVG 500 "figures/1.5/31.svg" $
    (group [ aSegment # rotate (18 * fromIntegral i) #: label (show i) <> white
           | i <- [1..6] ]) <+>
    aSegment # rotate (18 * 7) #: "7" <+>
    (group [ aSegment # rotate (18 * fromIntegral i) #: label (show (i - 7)) <> white
           | i <- [8..19] ]) <+>
    aSegment # rotate (18 * 20) #: "13" <+>
    aCircle #: invisible

  writeSVG 500 "figures/1.5/33.svg" $
    romashka <+> aSegment # rotate (18 * 13)

  writeSVG 500 "figures/1.5/34.svg" $
    romashka <+> aSegment # rotate (18 * 3)

  writeSVG 300 "figures/1.5/35.svg" $
    clock <+> hand 11 

  writeSVG 300 "figures/1.5/37.svg" $
    clock <+> hand 17

  writeSVG 200 "figures/1.5/38.svg" $
    let dow l i = aLabel
                  # on aCircle (-i * 1/7) # rotate 90
                  #: label l
    in group $ zipWith dow ["Пн","Вт","Ср","Чт","Пт","Сб","Вс"] [1..]

  writeSVG 300 "figures/1.5/51.svg" $
    let dec = regularPoly 10
        sc = group $ modularScale 10 dec
    in dec <+> sc
    
  writeSVG 450 "figures/1.5/53.svg" $
    group [ aSegment # scale 0.9 # rotate i <+>
            aLabel # at' i #: label (show i)
          | i <- asDeg <$> [10,20..360] ]

  writeSVG 450 "figures/1.5/54.svg" $
    anAngle 180 #: "#" <+> aPoint <+>
    aSegment <+> aSegment # rotate 180

  writeSVG 450 "figures/1.5/55.svg" $
    anAngle 90 #: "#" <+> aPoint <+>
    aSegment <+> aSegment # rotate 180 <+>
    aSegment # rotate 90
