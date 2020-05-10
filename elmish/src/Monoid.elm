module Monoid exposing (..)

type alias Monoid a = { mempty : a
                      , mappend : a -> a -> a }

monoid e a = { mempty = e, mappend = a }

msum = monoid 0 (+)
mmin = monoid (1/0) min
mmax = monoid (-1/0) max
mtuple m1 m2 =
    let f (a,b) (c,d) = (m1.mappend a c, m1.mappend b d)
    in monoid (m1.mempty, m2.mempty) f
       
fold m = List.foldl (m.mappend) (m.mempty)
foldMap m f = fold m << List.map f
