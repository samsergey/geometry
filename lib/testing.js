var failedParams = {},
    testCount = 0

function runTests(json, log = false, n=100) {
    if (json.skip) return
    if (json.run) { runTest(json, log); return }
    if (json.for) { property(json, log, n); return }
    if (json.suite)
    {
	if ((log || json.log) && json.name) console.group("Testing " + json.name)
	for(let t of json.suite)
	    runTests(t, log || json.log, json.number || n)
	if ((log || json.log) && json.name) console.groupEnd();
    }
}

function runTest(test, log) {
    if (test.skip) return
    if (log || test.log) console.log(`Testing ${test.name}...`)
    let result = test.run()
    if (!equal(result, test.result))
    {
	console.groupCollapsed(`%cTest %s failed`, 'color:red', test.name)
	console.log(`  expected: %c%s`, 'color:blue', test.result.toString(),)
	console.log(`  got:      %c%s`, 'color:blue', result.toString())
	console.groupEnd()
    } else
	if (log || test.log) console.log(`Passed.`)
}   

function property(data, log, n) {
    let options = {
	skip : false,
	assuming : [],
	hold : () => true,
	shrinks : 500,
	including : []
    }
    Object.assign(options, data)
    
    if (options['skip']) return

    if (log || options['log'])
	console.log(`Testing ${options['name']} ...`)
    let	res = checkProperty(options, n)
    if (!res.ok)
	reportFail(options['name'], res)
    if (res.ok && (log || options['log'])) {
	let passed = res.applied/res.samples
	let color = (passed < 0.25 ? 'red' :
		     passed < 0.5 ? '#500' :
		     'black')
	console.log('Ok. Total samples: %d, %cpassed assumptions: %s',
		    res.samples,
		    'color:'+color,
		    fmt.percent(passed))
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

function checkProperty (options, n) {
    let ac = {counter:0}, c = {counter:0}, sc = {counter:-2}
    let assumption = count(c)(x => conjunction(options.assuming).apply(null, x)),
	proposition = count(ac)(x => options.hold.apply(null, x)),
	test = implies(assumption, proposition),
	obligatory = Sequence.listProduct(options.including),
	samples = () => new Arbitrary().tuple(options.for).ascending(),
	shrinker = samples().shrink,
    	simplicity = r => r.sample ? samples().simplicity(r.sample) : Infinity,
	number = options.number || n

    let runc = count(sc)(run)
    let res = runc(samples().take(number))
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

	let sh = shrinker(h).take(options.shrinks)

	if (sh.isEmpty)
	    return {ok : ok, sample : h}
	
	return runc(sh, false, depth + 1, h)
    }
}

////////////////////////////////////////////////////////////

class Arbitrary extends Sequence {
    constructor (gen, shrink) {
	super(gen)
	this.shrink = shrink
	this.simplicity = () => Infinity
	this.simplest = undefined
    }

    static copy (obj) {
	let res = new Arbitrary()
	res.isEmpty = obj.isEmpty
	res.head = obj.head
	res.tailGen = obj.tailGen
	if (obj instanceof Arbitrary) {
	    res.elements = obj.elements && obj.elements.copy()
	    res.simplicity = obj.simplicity
	    res.simplest = obj.simplest
	    res.shrink = obj.shrink
	}
	return res
    }
    
    tuple (xs) {
	let res = Arbitrary.copy(new Sequence().tuple(xs))
	res.elements = xs
	res.simplicity = ns => ns.zipWith(xs, (n, e) => e.simplicity(n)).norm()
	res.simplest = xs.map(x => x.simplest)
	res.shrink = ns => {
	    let p = Gen.product(xs.zipWith(ns, (x, n) => () => x.shrink(n).generator()))
	    return new Sequence(p).ascendingBy(res.simplicity, res.simplest)
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
	res.simplicity = ns => res.elements.zipWith(destr(ns), (e, n) => e.simplicity(n)).norm()
	res.simplest = constr.apply(null, res.elements.map(x => x.simplest))
	res.shrink = x => {
	    let p = Gen.map(constr,
			    Gen.product(
				res.elements.zipWith(destr(x), (g, n) => () => g.shrink(n).generator())))
	    return new Sequence(p).ascendingBy(res.simplicity, res.simplest)
	}
	return res
    }

    ascending () {
	return this.setGen(Gen.ascendingBy(this.simplicity, this.simplest, this.generator()))
//	return this.ascendingBy(this.simplicity, this.simplest)
    }
}

function args (...x) { return new Arbitrary().tuple([...x]) }

class ArbitraryNumber extends Arbitrary {
    constructor (min = 0, max = 10) {
	super(Gen.rnd(min, max, roundUp(0.1)))
	this.min = min
	this.max = max
	this.prec = 0.1
	this.shrink = numShrink(this.prec, min)
	this.simplicity = abs
	this.simplest = 0
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
	res.tailGen = () => Gen.rnd(min, max, roundUp(this.prec))
	res.shrink = numShrink(this.prec, min)
	return res
    }

}

function ArbitraryCoordinates (min = -15, max = 15) {
    let x = new ArbitraryNumber().range(min, max).precision(0.25)
    let y = new ArbitraryNumber().range(min, max).precision(0.25)
    let xy = new Arbitrary().tuple([x,y])
    let res = new Arbitrary().tuple([x,y])
    res.simplicity = x => x.map(abs).sum()
    res.simplest = [0, 0]
    res.shrink = p => new Sequence(xy.shrink(p)
				   .take(500)
				   .list
				   .sorted((x,y) => x.norm() - y.norm()))
    return res.ascending()
}

const anyNum = () => new ArbitraryNumber().ascending()

function roundUp (p) {  return x => p*round (x/p) }

function numShrink(tol) {
    return b => Sequence
	.iterate(x => roundUp(tol)((x + b)/2), 0)
	.takeWhile(x => abs(x - b) >= tol)
}
