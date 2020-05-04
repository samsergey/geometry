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

window <<<  { equal, nequal, gequal, lequal, equalMod }
