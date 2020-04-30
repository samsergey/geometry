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
	console.groupCollapsed(`%cTest %s failed`, 'color:red', test.name)
	console.log(`  expected: %c%s`, 'color:blue', result.toString(),)
	console.log(`  got:      %c%s`, 'color:blue', test.result.toString())
	console.groupEnd()
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
    if (res.ok && options['log']) {
	console.log(`Ok. Test ${options['name']} passed.`)
	console.log('samples:            %d', res.samples)
	console.log('passed assumptions: %d (%d\%)',res.applied, (res.applied/res.samples)*100)
    }
}

function reportFail (name, res) {
    console.groupCollapsed(`%cProperty ${name} failed!`, 'color: red')
    console.log(`Counter example (${res.shrinks} shrinks):`)
    for(let x of res.sample)
	console.log('%c'+x.toString(),'color: blue')
    console.groupEnd()
    failedParams[name] = res.sample
}

function checkProperty (options) {
    let ac = {counter:0}, c = {counter:0}, sc = {counter:-2}
    let assumption = count(c)(x => conjunction(options.assuming).apply(null, x)),
	proposition = count(ac)(x => options.hold.apply(null, x)),
	test = implies(assumption, proposition),
	obligatory = Sequence.listProduct(options.including),
	samples = Arbitrary.tuple(options.for)

    let runc = count(sc)(run)
    let res = runc(samples.take(options.number))
    res.samples = c.counter
    res.applied = ac.counter
    res.shrinks = sc.counter
    return res

    function run(seq, ok = true, depth = 0, last) {
	let res = seq.dropWhile(test)
	if (res.isEmpty)
	    return {ok : ok, sample : last}

	let h = res.head
	
	if(!ok && res.tail.isEmpty)
	    return {ok : ok, sample : h}

	if (depth > options.shrinks)
	    return {ok : ok, sample : h}

	let sh = samples.shrink(h)
	    .take(options.shrinks)
	return runc(sh, false, depth + 1, h)
    }
}
var shs
////////////////////////////////////////////////////////////

class Arbitrary extends Sequence {
    constructor (gen, shrink) {
	super(gen)
	this.shrink = shrink
	this.weight = () => 1
    }

    static copy (obj) {
	let res = new Arbitrary()
	res.isEmpty = obj.isEmpty
	res.head = obj.head
	res.tailGen = obj.tailGen
	res.elements = obj.elements
	if (obj instanceof Arbitrary)
	    res.shrink = obj.shrink
	return res
    }
    
    static tuple (xs) {
	let res = Arbitrary.copy(Sequence.tuple(xs))
	res.shrink = ns => {
	    let p = Gen.product(xs.zipWith(ns, (x, n) => () => x.shrink(n).generator()))
	    return new Sequence(p)
	}
	return res
    }
    
    /** 
     * @example
     * args(anyNum).iso(point, p => [p.xy])
     * args(anyPoint, anyPoint).iso(line, l => [l.point(0), l.point(1)])
     **/
    iso (constr, destr) {
	var res = Arbitrary.copy(this.apply(constr))
	res.elements = this.elements.copy()
	res.shrink = x =>
	    new Sequence(
		Gen.map(constr,
			Gen.product(
			    res.elements.zipWith(destr(x), (g, n) => () => g.shrink(n).generator()))))
	return res
    }
}

function args (...x) { return Arbitrary.tuple([...x]) }

class ArbitraryNumber extends Arbitrary {
    constructor (min = 0, max = 1) {
	super(Gen.rnd(min, max, roundUp(0.1)))
	this.min = min
	this.max = max
	this.prec = 0.1
	this.shrink = numShrink(this.prec, min)
    }

    precision (prec) {
	let res = new ArbitraryNumber(this.min, this.max)
	res.prec = prec
	res.tailGen = () => Gen.rnd(this.min, this.max, roundUp(prec))
	res.shrink = numShrink(prec, this.min)
	return res
    }

    range (min, max) {
	let res = new ArbitraryNumber(min, max)
	res.prec = this.prec
	res.tailGen = () => Gen.rnd(min,max,roundUp(this.prec))
	res.shrink = numShrink(this.prec, min)
	return res
    }

}

function ArbitraryCoordinates (min = -10, max = 10) {
    let x = new ArbitraryNumber().range(min, max).precision(0.1)
    let y = new ArbitraryNumber().range(min, max).precision(0.1)
    let xy = Arbitrary.tuple([x,y])
    let res = Arbitrary.copy(xy)
    res.shrink = p => new Sequence(xy.shrink(p)
				   .take(1000)
				   .list
				   .sorted((x,y) => x.norm()-y.norm()))
    return res
}

const anyNum = () => new ArbitraryNumber()

function roundUp (p) {  return x => p*round (x/p) }

function numShrink(tol) {
    return b => Sequence
	.iterate(x => roundUp(tol)((x + b)/2), 0)
	.takeWhile(x => abs(x - b) >= tol)
}
