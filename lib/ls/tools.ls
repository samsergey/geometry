
#------------------------------------------------------------

dot = (a, b) -> sum zipWith (*), a, b
norm = (v) -> sqrt (v `dot` v)
vadd = (a, b) -> zipWith (+), a, b
vsub = (a, b) -> zipWith (-), a, b
flip = (v) -> map (0 -), v
vscale = (v, a) -> map (* a), v
cross = ([x1, y1], [x2, y2]) -> x1*y2 - x2*y1
normalize = (v) -> if n == 0 then v else vscale v (1/(norm v))


#------------------------------------------------------------
equal = (a, b) ->
  | Number.isFinite(a) and Number.isFinite(b)
    => abs(a - b) < 1e-10 || abs(a - b) < 1e-10*abs(a + b)
  | a instanceof Figure and a instanceof Figure
    => a.isEqual(b)
  | Array.isArray a and Array.isArray b
    => a.length == b.length and a.every((x,i) -> equal(x,b[i]))
  | otherwise
    => a == b

nequal = (a, b) ->  not (a `equal` b)
gequal = (a, b) ->  a > b or (a `equal` b)
lequal = (a, b) ->  a < b or (a `equal` b)
equalMod = (m) -> (a,b) -> (a %% m) `equal` (b %% m)

#------------------------------------------------------------
count = (c, f) --> (...x) ->
    c.counter++
    f(...x)

compare = (x,y) ->
    | (x `equal` y) => 'EQ'
    | x < y	=> 'LT'
    | x > y => 'GT'

implies = (P, Q) -> (...x) ->
    if P(...x) then Q(...x) else true

conjunction = (ps) -> (...xs) -> all ((p) -> p(...xs)), ps

#------------------------------------------------------------
window <<<  { norm, dot, cross, flip, vadd, vscale }
window <<<  { equal, nequal, gequal, lequal, equalMod, count }
window <<<  { compare, implies, conjunction }
