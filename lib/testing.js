class testSuite {
    constructor (name,tests) {
	this.name = name
	this.tests = tests
    }

    run () {
	console.group("Testing " + this.name)
	this.tests.forEach(t => t.run())
	console.groupEnd();
    }

    get length () { return this.tests.sum(t => t.length) }
}

function test(name, expr = true, expected = true, equality = equal) {
    this.length = 1
    this.run = function () {
	if (!equality(expr, expected))
	    console.log(`Fail! ${name}
  expected ${expected}
  got      ${expr}`)
    }
}

function trivialTest() {
    this.length = 0
    this.run = function () { }
}

function conjunction(ps) {
    return (...xs) => ps.every(p => p(...xs))
}

function Properties(data) {
    this.length = data.length
    this.run = function () { data.forEach(d => property(d)) }
}

var failedParams = {},
    testCount = 0

function property(data) {
    let options = {
	skip : false,
	assuming : [],
	require : () => true,
	number : 100,
	logging : false,
	shrinks : 100,
	including : []
    }
    Object.assign(options, data)
    
    if (options['skip']) return
    
    let	res = checkProperty(options)
    if (!res.ok)
	reportFail(options['name'], res)
    if (options['logging']) {
	console.log(options['name'])
	console.log('samples:            %d', res.samples)
	console.log('passed assumptions: %d (%d\%)',res.applied, (res.applied/res.samples)*100)
    }
}

function reportFail (name, res) {
    console.log(`%cProperty ${name} failed!`, 'color: red')
    console.log(`Counter example (${res.shrinks} shrinks):`)
    for(let x of res.sample)
	console.log('%c'+x.toString(),'color: blue')
    failedParams[name] = res.sample
}

var shs, trials = []
function checkProperty (options) {
    let ac = {counter:0}, c = {counter:0}, sc = {counter:0}
    let assumption = count(c)(x => conjunction(options.assuming).apply(null, x)),
	proposition = count(ac)(x => options.require.apply(null, x)),
	test = implies(assumption, proposition),
	inclusions = lList(cartesianProduct(options.including)),
	samples = any.tuple(options['for']),
	randoms = samples.generator()

    let runc = count(sc)(run)
    let res = runc(lTake(options.number, lSum([inclusions, randoms])))
    res.samples = c.counter
    res.applied = ac.counter
    res.shrinks = sc.counter
    return res

    function run(gen, ok = true, last, depth = 0) {
	let res = lDropWhile(test, gen).next()
	if (depth < options.shrinks) {
	    if (!ok && gen.next().done)
		return {ok:ok, sample:last}
	    if (!res.done){
		shs = lTake(options.shrinks, samples.shrink(res.value))
		return runc(shs, false, res.value, depth + 1)
	    }
	}
	return {ok:ok, sample:res.value}
    }
}

function* lList(lst) {
    for(let x of lst) yield x
}

function* lRandom(a, b, f = id) {
    for(;;) yield f(a + (b-a)*Math.random())
}

function* lInvoke(gs) {
    for(;;) yield gs.map(g => g.next().value)
}

function* empty () {}

/** Uses thunks to recreate generators */
function* lProduct(gs) {
    if (gs.length == 0)
	return
    else if (gs.length == 1)
	for(let x of gs[0]())
	    yield [x]
    else 
	for(let x of gs[0]()) 
	    for (let xs of lProduct(gs.tail()))
		yield [x].concat(xs)
}

function* lSum(gs) {
    if (gs.length == 0)	return
    if (gs.length == 1)	yield* gs[0]
    else {
	yield* gs[0]
	yield* lSum(gs.tail())
    }

}

function* lZip(gs) {
    for(;;) {
	let res = gs.map(g => g.next())
	if (res.some(x => x.done)) break
	yield res.map(x => x.value)
    }
}

function* lMap(f, g) {
    for(let x of g)
	yield f.apply(null, x)
}

function* lTake(n, g) {
    let i = n
    for(let x of g) {
	if (i-- <= 0) return
	yield x
    }
}

function* lDropWhile(p, gen) {
    let x = {done:false}
    while(!x.done) {
	x = gen.next()
	if (x.done) return
	if (!p(x.value)) break
    }
    if (!x.done) {
	yield x.value
	yield* geng
    }
}

class Arbitrary {
    constructor (gen, shrink) {
	this.generator = gen
	this.shrink = shrink
    }

    static tuple (xs) {
	let res = new Arbitrary()
	res.generator = () => lZip(xs.map(x => x.generator()))
	res.shrink = ns => lProduct(xs.zipWith(ns, (x, n) => () => x.shrink(n)))
	return res
    }

    iso (constr, destr) {
	var res = new Arbitrary()
	res.generator = () => lMap(constr, Arbitrary.tuple([this]).generator())
	res.shrink = xs => lMap(constr,
				lProduct(this.zipWith(destr(xs), (g, n) => () => g.shrink(n))))
	return res	
    }

    take (n) {
	var res = new Arbitrary()
	res.generator = () => lTake(n, this.generator())
	res.shrink = this.shrink
	return res
    }

    get list () {
	let g = this.generator()
	return [...g]
    }

    map (f) {
	var res = new Arbitrary()
	res.generator = () => lMap(f, Arbitrary.tuple([this]).generator())
	res.shrink = this.shrink
	return res
    }    
}

class ArbitraryNumber extends Arbitrary {
    constructor (min = 0, max = 1) {
	super()
	this.min = min
	this.max = max
	this.prec = 0.001
	this.generator = () => lRandom(min, max, roundUp(0.001))
	this.shrink = numShrink(this.prec, min)
    }

    precision (prec) {
	let res = new ArbitraryNumber(this.min, this.max)
	res.prec = prec
	res.generator = () => lRandom(this.min, this.max, roundUp(prec))
	res.shrink = numShrink(prec, this.min)
	return res
    }

    range (min, max) {
	let res = new ArbitraryNumber(min, max)
	res.prec = this.prec
	res.generator = () => lRandom(min,max,roundUp(this.prec))
	res.shrink = numShrink(this.prec, min)
	return res
    }

}

const anyNum =  new ArbitraryNumber()

function roundUp (p) {  return x => p*round (x/p) }

function numShrink(tol, min = 0) {
    return function* (b) {
	yield min
	let s = Math.sign(b)
	let x = min, max = roundUp(tol)(abs(b))
	while( abs(x - max) > tol ) {
	    x = roundUp(tol)((x + max)/2)
	    yield s*x
	}
    }
}
