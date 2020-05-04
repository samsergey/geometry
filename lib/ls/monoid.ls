Monoid = 
    sum:
      mappend: (x, y) -> x + y,
      mempty: 0
      
    product:
      mappend: (x, y) -> x * y
      mempty: 1

    string:
      mappend: (x, y) ->
        | !x => y
        | !y => x
        | otherwise => x + y
      mempty: ''

    path: (sep) ->
      mappend: (x,y) ->
        | !x => y
        | !y => x
        | otherwise => x + sep + y
      mempty: ''

    free:
      mappend: (x, y) -> x ++ y
      mempty: []

    max:
      mappend: max
      mempty: -Infinity

    min:
      mappend: min
      mempty: Infinity

    or:
      mappend: (x,y) -> x or y
      mempty: false

    and:
      mappend: (x,y) -> x and y
      mempty: true

    first:
      mappend: (x,y) -> x or y
      mempty: undefined

    second:
      mappend: (x,y) -> y or x
      mempty: undefined

    composition:
      mappend: (f, g) -> (f || id) . (g || id)
      mempty: id

    object:
      mappend: (x,y) -> Object.assign(Object.assign({},x),y)
      mempty: {}

Monoid.keep = Monoid.second
Monoid.replace = Monoid.first
Monoid.fold = (m, list) -> list.reduce m.mappend, m.mempty

window <<< { Monoid }
