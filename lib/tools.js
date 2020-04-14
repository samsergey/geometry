function memoized(f)
{
    var dict = {}
    return (...x) => dict[x] || (dict[x] = f(...x))
}

function nest(f,x,n)
{
    res = x
    for(var i = 0; i < n; i++)
	res = f(res,i)
    return res
}

function nestList(f,x,n)
{
    res = [x]
    for(var i = 0; i < n; i++)
	res.unshift(f(res[0]))
    return res.reverse()
}

function nestWhileList(f,x,test,n)
{
    res = [x]
    for(var i = 0; i < n; i++) {
	res.unshift(f(res[0]))
	if (!test(res[0]))
	    return res.reverse() 
    }
    return res.reverse()
}


function fixedPoint(f,x,n,compare,eq) {
    if (n == 0) return x
    n = n || 100
    compare = compare || (x => x.toString())
    eq = eq || ((x,y) => x == y)
    var res = x
    for (var i = 0; i < n; i++) {
	res = f(x)
	if (eq(compare(res), compare(x))) break
	x = res
    }
    return x
}

// Array

Array.prototype.copy = function() {
    return [].concat(this)
}

Array.prototype.equal = function(lst) {
    return this.every((x,i)=>x==lst[i])
}

Array.prototype.head = function() {
    return this[0]
}

Array.prototype.tail = function() {
    return this.slice(1,this.length)
}

Array.prototype.drop = function(n) {
    return this.slice(n,this.length)
}

Array.prototype.take = function(n) {
    return this.slice(0,n)
}

Array.prototype.most = function() {
    return this.slice(0,this.length-1)
}

Array.prototype.last = function() {
    return this.slice(this.length-1,this.length)[0]
}

Array.prototype.skip = function(i) {
    return this.slice(0,i-1).concat(this.slice(i))
}


Array.prototype.rotate = function() {
    return [this.last()].concat(this.most())
}

Array.prototype.rotatel = function() {
    return this.tail().concat([this.head()])
}


Array.prototype.close = function() {
    return this.concat([this[0]])
}

Array.prototype.flatMap = Array.prototype.mapappend 
Array.prototype.mapappend = function(f) {
    var res = []
    for(var i = 0; i < this.length; i++)
	res = res.concat(f(this[i],i))
    return res
}

Array.prototype.sorted = function(order) {
    var res = [].concat(this)
    res.sort(order || ascending)
    return res
}

var ascending = (x,y) => Number(x) - Number(y)
var descending = (x,y) => y - x
var on = f => g => (x,y) => g(f(x),f(y))
var fst = x => x[0]
var snd = x => x[1]
var length = x => x.length

var Sum = {mappend:(x,y) => x+y, mempty:0}
var Product = {mappend:(x,y) => x*y, mempty:1}
var Free = {mappend:(x,y) => x.concat(y), mempty:[]}

Array.prototype.foldMap = function(f,m)
{
    return this.reduce((res,x) => m.mappend(res, f(x)), m.mempty)
}

Array.prototype.fold = function(m)
{
    return this.foldMap(id,m)
}


Array.prototype.outer = function(lst,f)
{
    var res = []
    this.forEach(i => lst.forEach(j => res.push([i,j])))
    return res
}


Array.prototype.sum = function(f,s0)
{
    var f = f || id
    return this.reduce((res,x,i) => res + f(x,i), s0 || 0)
}

Array.prototype.mean = function(f)
{
    var f = f || (x => 1)
    return this.reduce((res,x,i) => res + x*f(x,i), 0)/this.length
}


Array.prototype.reversed = function()
{
    var res = [].concat(this)
    res.reverse()
    return res
}


Array.prototype.foldList = function(f,x0)
{
    var res = [], s = x0 || this[0]
    res.push(s)
    this.forEach(x => res.push(s=f(s,x)))
    return res
}

Array.prototype.accumsum = function (f,x0)
{
    var F = f || (x => x)
    var res = [x0 || 0]
    this.forEach((x,i)=>res.push(F(x,i)+res.last()))
    return res
}

Array.prototype.ema = function (a)
{
    return this.foldList((x,y)=>x*(1-a)+a*y);
}

Array.prototype.differences = function (dx)
{
    var res = []
    for(var i = dx;i < this.length; i++)
	res.push(this[i]-this[i-dx])
    return res
}

Array.prototype.positions = function (p)
{
    var res = []
    for(var i = 0;i < this.length; i++)
	if (p(this[i])) res.push(i)
    return res
}

Array.prototype.position = function (p)
{
    for(var i = 0;i < this.length; i++)
	if (p(this[i])) return i
    return null
}

function binarySearch(pts,x)
{
    const n = pts.length-1
    if (x < pts[0][0]) return [pts[0],pts[1]]
    if (x > pts[n][0]) return [pts[n-1],pts[n]]
    var a = 0, b = n, c
    while (b-a > 1) {
	c = floor((b+a)/2)
	if (pts[a][0] <= x && x <= pts[c][0])
	    b = c
	else
	    a = c
    }
    return [pts[a],pts[b]]
}


Array.prototype.find = function (p)
{
    var i = this.position(p)
    return i && this[i]
}


Array.prototype.zipWith = function(lst,f)
{
    var res = [], N = min(this.length,lst.length)
    for(var i = 0; i < N; i++)
	res.push(f(this[i], lst[i]))
    return res
}

Array.prototype.zip = function(lst)
{
    var res = [], N = min(this.length,lst.length)
    for(var i = 0; i < N; i++)
	res.push([this[i], lst[i]])
    return res
}

Array.prototype.removeAt = function(i)
{
    return this.slice(0,i).concat(this.slice(i+1,this.length))
}

Array.prototype.sample = function()
{
    return this[Math.floor(Math.random()*this.length)]
}


Array.prototype.add = function (x,gt)
{
    var gt = gt || ((x,y) => x > y)
    var n = this.length
    if (n == 0) {
	this.push(x)
	return this
    }
    var res = [],y
    for(var i = 0; i < n; i++)
    {
	y = this.shift()
	if (y.toString() == x.toString())
	{
	    res.push(y)
	    break
	}
	if (gt(y, x))
	{
	    res.push(x)
	    res.push(y)
	    break
	}
	res.push(y)
    }
    if (i == n) res.push(x) 
    return res.concat(this)
}

Array.prototype.unique = function()
{
    return this.reduce((res,l) => res.add(l),[])
}


Array.prototype.merge = function(right)
{
    let result = []
    let indexLeft = 0
    let indexRight = 0
    while (indexLeft < this.length && indexRight < right.length) {
	if (this[indexLeft] < right[indexRight]) {
	    result.push(this[indexLeft])
	    indexLeft++
	} else {
	    result.push(right[indexRight])
	    indexRight++
	}
    }
    return result.concat(this.slice(indexLeft)).concat(right.slice(indexRight))
}

Array.prototype.filter = function(f)
{
    var res = []
    for(var i = 0; i < this.length; i++)
	if (f(this[i]))
	    res.push(this[i])
    return res
}

Array.prototype.repeat = function(n)
{
    return d3.range(n).mapappend(() => this)
}

Array.prototype.dot = function(v)
{
    var res = 0, i = 0
    this.forEach(x => res += x*v[i++])
    return res
}

Array.prototype.max = function()
{
    return this.reduce((res,x) => max(res,x))
}

Array.prototype.min = function()
{
    return this.reduce((res,x) => min(res,x))
}


function repeat(n,action)
{
    for(var i = 0; i < n; i++)
	action(i)
}

function transpose(a) {
    return Object.keys(a[0]).map(function(c) {
        return a.map(function(r) { return r[c]; });
    });
}

Array.prototype.transpose = function()
{
    return transpose(this.copy())
}

Array.prototype.EMA = function(alpha)
{
    var res=[]
    var s = this[0]
    res.push(s)
    for(var i=1; i < this.length-1; i++)
    {
	s = s*(1-alpha)+alpha*this[i]
	res.push(s)
    }
    return res
}


Array.prototype.partition = function(n)
{
    var res = []
    for(var i = 0; i < this.length; i+=n)
    {
	res.push([])
	for(var j = 0; j < n; j++)
	    if (i+j<this.length)
		res[i/n].push(this[i+j])
    }
    return res
}

Array.prototype.runs = function()
{
    if (this.length==0) return []
    if (this.length==1) return [this]
    var x = this[0]
    var res = [[x]]
    for(var i = 0; i < this.length; i++)
    {
	if (this[i] == x)
	    res[0].push(x)
	else
	    res.unshift([x = this[i]])
    }
    return res.reversed()
}

Array.prototype.splitBy = function(p)
{    
    var res = [[]]
    for(var i = 0; i < this.length; i++)
    {
	if (!p(this[i]))
	    res[0].push(this[i])
	else
	    res.unshift([])
    }
    return res.reversed()
}

Array.prototype.splitOn = function(el)
{
    return this.splitBy(x => x == el)
}

Array.prototype.convolve = function(lst)
{
    var res = []
    for(var j=0; j<lst.length+this.length-1; j++) {
	res[j]=0
	for(var i=0; i<this.length; i++) 
	    res[j] += (this[i] || 0)*(lst[lst.length-1-j+i] || 0)
    }
    return res
}

Array.prototype.correlate = function(lst)
{
    var res = []
    for(var j=0; j<this.length-lst.length; j++) {
	res.push(this.slice(j,j+lst.length).dot(lst))
    }
    return res
}

Array.prototype.rsum = function(f)
{
    return go(this.map(f))
    function go(lst)
    {
	if (lst.length == 0) return 0
	if (lst.length == 1) return lst[0]
	if (lst.length == 2) return lst[0]+lst[1]
	return go(lst.slice(0,Math.floor(lst.length/2)))+go(lst.slice(Math.floor(lst.length/2),lst.length))
    }
}


Array.prototype.distance = function (v)
{
    return sqrt(this.sum((x,i)=>(x-v[i])**2))
}

Array.prototype.sparce = function (n)
{
    var L = 0
    for(var i = 1; i < this.length; i++)
	L += this[i].distance(this[i-1])
    var dl = L/n, res = [this[0]], l = 0
    for(var i = 1; i < this.length; i++)
    {
	l += this[i].distance(this[i-1])
	if (l >= dl || i == this.length-1) {
	    res.push(this[i])
	    l=0
	}
    }
    return res
}

function Queue(N)
{
    this.data = []
    this.add = function (x)
    {
	if (this.data.length < N)
	    this.data.push(x)
	else
	{
	    this.data.push(x)
	    this.data.shift(x)
	}
	return this
    }
}

function center(s, n,x)
{
    var l = s.length
    return s.padStart(l+(n-l)/2,x).padEnd(n,x)
}

function getUrlVars() {
    var vars = {};
    var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
        vars[key] = value;
    });
    return vars;
}

function pageBreak() {
    document.write('<div class="pagebreak"> </div>')
}

Array.prototype.norm = function()
{
    return sqrt(this.dot(this))
}

Array.prototype.scale = function(a)
{
    return this.map(x => a*x)
}

Array.prototype.flip = function(a)
{
    return this.map(x => -x)
}

Array.prototype.vadd = function (a)
{
    return this.map((x,i) => a[i]+x)
}

Array.prototype.vsub = function (a)
{
    return this.map((x,i) => x - a[i])
}

Array.prototype.normalize = function()
{
    if (this.norm() == 0) return this
    return this.scale(1/this.norm())
}

function equal(a,b) {
    if (Number.isFinite(a) && Number.isFinite(b))
	return abs(a - b) < 1e-13
    if (a instanceof Figure && a instanceof Figure)
	return a.isEqual(b)
    if (Array.isArray(a) && Array.isArray(b))
	return a.length == b.length && a.every((x,i) => equal(x,b[i]))
    return a == b;
}

function nequal(a,b) {
    return !equal(a,b)
}

function equalMod(m,a,b)
{
    return (Number.isFinite(a) && Number.isFinite(b)) &&
	abs(a - b) % m <= 1e-13;
}

class testSuite {
    constructor (name,tests) {
	this.name = name
	this.tests = tests
    }

    run () {
	console.log("Running " + this.name)
	this.tests.forEach(t => t.run())
    }

    get length () { return this.tests.sum(t => t.length) }
}

function test(name, expr = true, expected = true, equality = equal) {
    this.length = 1
    this.run = function (n) {
	if (!equality(expr, expected))
	    console.log(`Fail! ${name}
  expected ${expected}
  got      ${expr}`)
    }
}

