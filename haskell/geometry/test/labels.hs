{-# Language OverloadedStrings #-}

import Geometry
import Data.Complex

ss = group $ take 6 $ iterate (rotate 60) $ aSegment #: "a"

rs = group $ take 5 $ iterate (rotate 72) $ aRay #: "b"

ps = group $ [ point (x,y) #: "A" <> loffs (x:+y)
             | x <- [-1,0,1] 
             , y <- [-1,0,1] ]

as = anAngle 72 #: "#" <+>
     (anAngle 72 #: "β" <> mark 2 # rotate 72)

ch = ss <+>
     ((rs <+> as) # rotate 3 # translate (0, 3)) <+>
     (ps # translate (3, 0))

main = writeSVG 400 "labels.svg" ch
