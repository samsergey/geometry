var failedParams = {},
    testCount = 0

function runTests(json) {
    if (json.skip) return
    if (json.run) { runTest(json); return }
    if (json.for) { property(json); return }
    console.group("Testing " + json.name)
    if (json.suite)
	    for(let t of json.suite)
		runTests(t)
    console.groupEnd();
}

function runTest(test) {
    if (test.skip) return
    let result = test.run()
    if (!equal(result, test.result))
    {
	console.log(`%cTest %s failed`, 'color:red', test.name)
	console.log(`  expected: %c%s`, 'color:blue', result.toString(),)
	console.log(`  got:      %c%s`, 'color:blue', test.result.toString())
    }
}   

function property(data) {
    let options = {
	skip : false,
	assuming : [],
	hold : () => true,
	number : 100,
	log : false,
	shrinks : 100,
	including : []
    }
    Object.assign(options, data)
    
    if (options['skip']) return
    
    let	res = checkProperty(options)
    if (!res.ok)
	reportFail(options['name'], res)
    if (options['log']) {
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

function checkProperty (options) {
    let ac = {counter:0}, c = {counter:0}, sc = {counter:0}
    let assumption = count(c)(x => conjunction(options.assuming).apply(null, x)),
	proposition = count(ac)(x => options.hold.apply(null, x)),
	test = implies(assumption, proposition),
	inclusions = Sequence.listProduct(options.including),
	samples = Arbitrary.tuple(options.for),
	randoms = samples.generator()

    let runc = count(sc)(run)
    let res = runc(gTake(options.number, gSum([inclusions, randoms])))
    res.samples = c.counter
    res.applied = ac.counter
    res.shrinks = sc.counter
    return res

    function run(gen, ok = true, last, depth = 0) {
	let res = gDropWhile(test, gen).next()
	if (depth < options.shrinks) {
	    if (!ok && gen.next().done)
		return {ok:ok, sample:last}
	    if (!res.done){
		let shs = gTake(options.shrinks, samples.shrink(res.value))
		return runc(shs, false, res.value, depth + 1)
	    }
	}
	return {ok:ok, sample:res.value}
    }
}

function* gList(lst) {
    for(let x of lst) yield x
}

function* gRandom(a, b, f = id) {
    for(;;) yield f(a + (b-a)*Math.random())
}

function* gInvoke(gs) {
    for(;;) yield gs.map(g => g.next().value)
}

function* gEmpty () {}

/** Uses thunks to recreate generators */
function* gProduct(gs) {
    if (gs.length == 0)
	return
    else if (gs.length == 1)
	for(let x of gs[0]())
	    yield [x]
    else 
	for(let x of gs[0]()) 
	    for (let xs of gProduct(gs.tail()))
		yield [x].concat(xs)
}

function* gSum(gs) {
    if (gs.length == 0)	return
    if (gs.length == 1)	yield* gs[0]
    else {
	yield* gs[0]
	yield* gSum(gs.tail())
    }

}

function* gZip(gs) {
    for(;;) {
	let res = gs.map(g => g.next())
	if (res.some(x => x.done)) break
	yield res.map(x => x.value)
    }
}

function* gMap(f, g) {
    for(let x of g)
	yield f.apply(null, x)
}

function* gTake(n, g) {
    let i = n
    for(let x of g) {
	if (i-- <= 0) return
	yield x
    }
}

function* gDropWhile(p, gen) {
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

class Sequence {
    constructor (gen) {
	if (Array.isArray(gen))
	    this.generator = gList(gen)    
	this.generator = () => gen
    }

    copy () {
	let res = new Sequence()
	res.generator = this.generator
	res.elements = this.elements
	return res
    }
    
    take (n) {
	var res = this.copy()
	res.generator = () => gTake(n, this.generator())
	return res
    }

    get list () {
	let g = this.generator()
	return [...g]
    }

    /** 
     * @example
     * args(anyNum).apply(sin)
     * args(anyPoint, anyPoint).apply(line)
     **/
    apply (f) {
	var res = this.copy()
	res.generator = () => gMap(f, this.generator())
	return res
    }
    
    map (f) {
	var res = this.copy()
	res.generator = () => gMap(f, Arbitrary.tuple([this]).generator())
	return res
    }    

    static tuple (seqs) {
	var res = new Arbitrary()
	res.elements = seqs
	res.generator = () => gZip(seqs.map(x => x.generator()))
	return res
    }

    concat (seq) {
	var res = this.copy()
	res.generator = () => gSum([this.generator(), seq.generator()])
	return res
    }    

    static sum(seqs) {
	return seqs.foldl((el,res) => el.concat(res))
    }
    
    product (seq) {
	var res = this.copy()
	res.generator = () => gProduct([this.generator, seq.generator])
	return res
    }    

    static listProduct(lists) {
	var res = new Arbitrary()
	res.generator = () => gProduct(lists.map(l => new Sequence(l).generator))
	return res
    }
}

function seq (...xs) {
    return new Sequence([...xs])
}

class Arbitrary extends Sequence {
    constructor (gen, shrink) {
	super()
	this.generator = gen
	this.shrink = shrink
    }

    copy () {
	let res = new Arbitrary()
	res.generator = this.generator
	res.elements = this.elements
	res.shrink = this.shrink
	return res
    }
  
    static tuple (xs) {
	let res = new Arbitrary()
	res.elements = xs
	res.generator = () => gZip(xs.map(x => x.generator()))
	res.shrink = ns => new Sequence(gProduct(xs.zipWith(ns, (x, n) => () => x.shrink(n).generator())))
	return res
    }
   
    /** 
     * @example
     * args(anyNum).iso(point, p => [p.xy])
     * args(anyPoint, anyPoint).iso(line, l => [l.point(0), l.point(1)])
     **/
    iso (constr, destr) {
	var res = this.copy()
	res.elements = this.elements.copy()
	res.generator = () => gMap(constr, this.generator())
	res.shrink = x => new Sequence(
	    gMap(constr, gProduct(res.elements.zipWith(destr(x), (g, n) => () => g.shrink(n).generator()))))
	return res	
    }
}

function args (...x) { return Arbitrary.tuple([...x]) }

class ArbitraryNumber extends Arbitrary {
    constructor (min = 0, max = 1) {
	super()
	this.min = min
	this.max = max
	this.prec = 0.001
	this.generator = () => gRandom(min, max, roundUp(0.001))
	this.shrink = numShrink(this.prec, min)
    }

    precision (prec) {
	let res = new ArbitraryNumber(this.min, this.max)
	res.prec = prec
	res.generator = () => gRandom(this.min, this.max, roundUp(prec))
	res.shrink = numShrink(prec, this.min)
	return res
    }

    range (min, max) {
	let res = new ArbitraryNumber(min, max)
	res.prec = this.prec
	res.generator = () => gRandom(min,max,roundUp(this.prec))
	res.shrink = numShrink(this.prec, min)
	return res
    }

}

const anyNum =  new ArbitraryNumber()

function roundUp (p) {  return x => p*round (x/p) }

function numShrink(tol, min = 0) {
    var gen = function* (b) {
	yield 0
	let s = Math.sign(b)
	let x = Math.max(min,0), max = roundUp(tol)(abs(b))
	while( abs(x - max) > tol ) {
	    x = roundUp(tol)((x + max)/2)
	    yield s*x
	}
    }
    return b => new Sequence(gen(b))
}

////////////////////////////////////////////////////////////

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
