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
	'skip' : false,
	'assuming' : [],
	'require' : () => true,
	'number' : 500,
	'logging' : false,
	'shrinks' : 100
    }
    Object.assign(options, data)
    
    if (options['skip']) return
    
    let	res = checkProperty(options)
    if (!res.ok)
	reportFail(options['name'], res.sample, res.shrinks)
    if (options['logging']) {
	console.log(options['name'])
	console.log('applied: %d (%d\%)',res.applied, (res.applied/options['number'])*100)
    }
}

var shs, trials = []
function checkProperty (options) {
    let ac = 0
    let assumption = x => conjunction(options.assuming).apply(null, x),
	proposition = x => {ac++; return options.require.apply(null, x)},
	test = x => assumption(x) ? proposition(x) : true,
	samples = any.tuple(options['for']),
	n = options.number,
	ns = options.shrinks

    return run(lTake(n, samples.generator()), true, 0)

    function run(gen, ok, shc, last) {
	let res = lDropWhile(test, gen).next()
	if (shc < ns) {
	    if (!ok && gen.next().done)
		return {ok:ok, sample:last, 'applied':ac, 'shrinks':shc}
	    if (!res.done){
		shs = lTake(ns, samples.shrink(res.value))
		return run(shs, false, shc + 1, res.value)
	    }
	}
	return {ok:ok, sample:res.value, 'applied':ac, 'shrinks':shc}
    }
}

function reportFail (name, sample, shrinks) {
    console.log(`%c Property ${name} failed!`,  'color: red')
    console.log(`Counter example (shrinks: ${shrinks}):`)
    sample.forEach(x => console.log('%c'+x.toString(),'color: blue'))
    failedParams[name] = sample
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
	yield []
    else 
	for(let x of gs[0]()) 
	    for (let xs of lProduct(gs.tail()))
		yield [x].concat(xs)
}

function* lSum([g, ...gs]) {
    if (gs.length == 0)
	yield* g
    else {
	yield* g
	yield* lSum([...gs])
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
