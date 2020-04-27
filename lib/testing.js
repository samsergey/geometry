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
	'special' : true,
	'random' : true,
	'assuming' : [],
	'require' : () => true,
	'number' : 500,
	'logging' : false
    }
    Object.assign(options, data)
    
    if (options['skip']) return

    let x = any.tuple(options['for']),
	spec = options['special'] ? specials(x) : function* () {},
	rand = options['random'] ? randomSamples(x) : function* () {}

    let res = runProperty(concat(spec, take(options['number'],rand)),		      
			  conjunction(options['assuming']),
			  options['require'],0,0)
    if (!res.ok)
	reportFail(options['name'], res.sample)
    if (options['logging']) {
	console.log(options['name'])
	console.log('samples: %d',res.counter)
	console.log('applied: %d (%d\%)',res.applied, (res.applied/res.counter)*100)
    }
}

function cartesianProduct (lists, f = (...xs) => [...xs]) {
    return lists.most()
	.foldr(
	    (el,res) => el.mapappend(
		x => res.mapappend(
		    ys => [[x].concat(ys)]))
	    , lists.last().map(x => [x]))
	.map(xs => f.apply(null,xs))
}

function compare(x,y) {
    return equal(x, y) ? 'EQ'
	: x < y	? 'LT'
	: 'GT'
}

function* emptyGen() { }

function* specials(g) {
    for(let i = 0; i< g.special.length; i++)
	yield g.special[i]
}

function* randomSamples(g) {
    for(;;) yield g.sample()
}

function* take(n, g) {
    for(let i=0; i < n; i++) yield g.next().value
}

function* concat(g1, g2) {
    yield* g1
    yield* g2
}

function implies(P,Q) {
    return P ? Q : true
}

function runProperty (samples, assumption, property, _sc, _ac) {
    let s, sc = _sc, ac = _ac
    for(s of samples) {
	sc++
	if (assumption.apply(null, s)) {
	    ac ++
	    if (property.apply(null, s))
		continue
	    else {
		var sh = s.shrink || empty
		return shrink(sh.apply(null, s), assumption, property,sc,ac)
	    }
	}
    }
    return {ok:true, sample:[]}
}

function shrink(samples, assumption, property,sc,ac) {
    console.log('entered shrink')
    return samples
    let s = samples.next()
    if (s.done)
	return {ok:false, sample:s.value, 'count':sc, 'applied':ac}
    return runProperty (samples, assumption, property,sc,ac)
}


function reportFail (name, sample) {
    console.log(`Property ${name} failed!`)
    sample.forEach(x => console.log(x))
    failedParams[name] = sample
}
