destr = (g) ->
    x = g.next!
    if x.done
    then null
    else {head: x.value, tail: g}
    
rnd = (a, b, f = id) !->*
    loop
        yield f(a + (b - a)*Math.random())

bind = (g, proc) !->*
    until ((x = g.next!).done)
        yield from proc(x.value)
                
/** Uses thunks to recreate generators */
product = (gs) !->*
    yield from switch
        | gs.length == 0 => !->*
        | gs.length == 1 =>
            (gs.0)! `bind` (x) !->* yield [x]
        | gs.length >= 0 =>
            (gs.0)! `bind` (x) !->*
                yield from (product tail gs) `bind` (xs) !->*
                    yield [x] ++ xs

sum = (gs) !->*
    | gs.length == 0 => return
    | gs.length >= 0 =>
        yield from gs.0
        yield from sum tail gs

zip = (gs) !->*
    loop 
        res = map ((g) -> g.next!), gs
        break if (any ((x) -> x.done), res)
        yield map ((x) -> x.value), res

apply-g = (f, g) !->*
    until ((x = g.next!).done)
        yield f.apply(null, x)


take = (n, g) !->*
    i = n
    until (i-- <= 0) or ((x = g.next!).done)
        yield x.value

list = (lst) !->*
    for x in lst
        yield x

cons = (x,g) !->*
    yield x
    yield from g

dropWhile = (p, g) !->*
    until ((x = g.next!).done or !(p x.value))
        void
    yield x.value
    yield from g

takeWhile = (p, g) !->*
    while (!(x = g.next!).done && p(x.value))
        yield x.value if p x.value

iterate = (f, x0) !->* 
    yield x = x0
    loop
        yield x = f(x)g

filter-g = (p, g) !->*
    until (x = g.next!).done
        yield x if p x

tail-g = (g) !->*
    g.next!
    yield from g

G =
    destr: destr
    tail: tail-g
    rnd: rnd
    bind: bind
    product: product
    sum: sum
    take: take
    zip: zip
    apply: apply-g
    list: list
    dropWhile: dropWhile
    takeWhile: takeWhile
    iterate: iterate
    filter: filter-g
    
window <<< {G}
                        

    
    
#     pairs : function* (xs, ys) {
	
# 	function* bind(g, f) {
# 	    let xs = Gen.destr(g)
# 	    if (xs)
# 		yield* merge(f(xs.head), bind(xs.tail, f))
# 	}
	
# 	function* merge(xs, ys) {
# 	    let dxs = Gen.destr(xs)
# 	    if (!dxs)
# 		yield* ys
# 	    else 
# 		yield* Gen.cons(dxs.head, merge(ys, dxs.tail))
# 	}	

# 	yield* bind(xs(), function* (x) {
# 	    yield* bind(ys(), function* (y) {
# 		yield [x,y] })})
#     },

#     ascendingBy : function* (f, x0, g) {
# 	let comp = (x,y) => f(x) > f(y)
# 	let x, N = 32, buffer = [x0]
# 	for(let i = 0; i < N; i++) {
# 	    x = g.next()
# 	    if (x.done) break
# 	    buffer = insert(buffer, x.value, f)
# 	}
# 	yield* Gen.list(buffer)
# 	for(x of g)  {
# 	    buffer = insert(buffer, x, f)
# 	    yield buffer.shift()
# 	}
#     },

#     accum : function* (f, x0, g) {
# 	var res = x0
# 	for(x of g)  {
# 	    res = f(res, x)
# 	    yield(res)
# 	}
#     }
# }
