
////////////////////////////////////////////////////////////

function ContinuousDistribution(par)
{
    this.parameters = par
    this.isDistribution = true
    this.isDiscrete = false
    this.support   = undefined
    this.PDF = undefined
    this.CDF       = cdf(this,x => integrate(this.PDF,[0,x]))
    this.quantile  = quantileFun(q => newton(x => this.CDF(x) - q,this.PDF,this.mean))
    this.mean      = undefined
    this.variance  = undefined
    this.mode      = undefined
    this.generator = undefined
    this.samples   = n => d3.range(n).map(this.generator).filter(x => !isNaN(x))
    this.fromHistogram  = undefined
    this.fromList = lst => this.fromHistogram(new Histogram().fromList(lst))
}

Object.defineProperty
(ContinuousDistribution.prototype, "stdev",
 { get : function ()
   {
       return sqrt(this.variance)
   }
 }
)

Object.defineProperty
(ContinuousDistribution.prototype, "median",
 { get : function ()
   {
       return this.quantile(1/2)
   }
 }
)

Object.defineProperty
(ContinuousDistribution.prototype, "sample",
 { get : function ()
   {
       return this.generator()
   }
 }
)

ContinuousDistribution.prototype.LorentzCurve = function (lst)
{
    var x0 = lst[0]
    var res = lst.map(x=>[integrate(t=>this.PDF(t),[x0,x]),integrate(t=>t*this.PDF(t),[x0,x])])
    var xmax = res.last()[0], ymax = res.last()[1]
    return res.map(([x,y])=>[x/xmax,y/ymax])
}

function Gini(l)
{
    var res = 1
    for(var i = 1;i< l.length;i++)
	res -= (l[i][0]-l[i-1][0])*(l[i][1]+l[i-1][1])
    return res
}

////////////////////////////////////////////////////////////

function DiscreteDistribution(par)
{
    this.parameters = par
    this.isDistribution = true
    this.isDiscrete = true
    this.support   = undefined
    this.PMF = undefined
    this.CDF       = cdf(this,x => range(0,x).sum(this.PMF))
    this.quantile  = quantileFun(q => bisection(this.CDF,0,1,x => x <= q)[0])
    this.mean      = undefined
    this.variance  = undefined
    this.mode      = undefined
    this.generator = undefined
    this.samples   = n => d3.range(n).map(this.generator)
    this.fromHistogram  = undefined
    this.fromList = lst => this.fromHistogram(new Histogram().fromList(lst))
}

Object.defineProperty
(DiscreteDistribution.prototype, "stdev",
 { get : function ()
   {
       return sqrt(this.variance)
   }
 }
)

Object.defineProperty
(DiscreteDistribution.prototype, "median",
 { get : function ()
   {
       return this.quantile(1/2)
   }
 }
)

Object.defineProperty
(DiscreteDistribution.prototype, "sample",
 { get : function ()
   {
       return this.generator()
   }
 }
)

DiscreteDistribution.prototype.LorentzCurve = function (lst)
{
    var res = transpose([lst.accumsum(this.PMF),lst.accumsum(t=>t*this.PMF(t))])
    var xmax = res.last()[0], ymax = res.last()[1]
    return res.map(([x,y])=>[x/xmax,y/ymax])
}


isDistribution = function (x) {
    return (typeof x == 'object') && x.isDistribution || false
}

////////////////////////////////////////////////////////////

function quantileFun(f)
{
    return x =>
	(x >= 0 && x < 1) ? f(x) : undefined
}

function pdf(self,f)
{
    return x =>
	(x >= self.support[0] && x <= self.support[1]) ? f(x) : 0
}

function cdf(self,f)
{
    return x =>
	(x <= self.support[0] ? 0 :
	 x >= self.support[1] ? 1 :
	 f(x))
}

////////////////////////////////////////////////////////////


function binFunction(s) {
    return x => floor(x/s)*s + s/2
}

function Histogram(s)
{
    this.isHistogram = true
    this.parameters = 'empiric'
    this.isDistribution = true
    this.isDiscrete = true
    this.step = s || 1
    this.data = {}
    this.bins = []
    this.total = 0
    this.number = 0
    this.mode = undefined
    this.antimode = undefined
    this.minBin = 1/0
    this.maxBin = -1/0
    this.minValue = 1/0
    this.maxValue = 0
    var binF = (this.step == 'none') ? id : binFunction(this.step)
        
    this.add = function (x,n)
    {
	var n = !n ? 1 : n

	var bin = binF(x)
	this.bins = this.bins.add(bin)
	this.total += x*n
	this.number += n
	this.data[bin] = (this.data[bin] || 0) + n
	this.maxBin = max(this.maxBin,bin)
	this.minBin = min(this.minBin,bin)
	this.maxValue = max(this.maxValue,this.data[bin])
	this.minValue = min(this.minValue,this.data[bin])
	if (this.mode == undefined) this.mode = bin
	if (this.data[this.mode] < this.data[bin])
	    this.mode = bin
	if (this.antimode == undefined) this.antimode = bin
	if (this.data[this.antimode] > this.data[bin])
	    this.antimode = bin
	this.quantileData = null
	return this
    }

    this.fromList = function (lst)
    {
	lst.forEach(x => this.add(x))
	return this
    }

    this.fromMap = function (m)
    {
	for(b in m)
	    if (!isNaN(Number(b)))
		this.add(Number(b),m[b])
	return this
    }
    
    this.fromDistribution = function (dist,n)
    {
	return this.fromList(dist.samples(n))
    }

    
    this.addList = function(data)
    {
	data.forEach(x => this.add(x))
	return this
    }
    
    this.PMF = function(x)
    {
	var bin
	if (x < this.minBin-this.step/2)
	    return 0
	if (x > this.maxBin+this.step/2)
	    return 0
	if (this.step == 'none')
	    bin = x
	else
	    bin = floor(x/this.step)*this.step+this.step/2
	return (this.data[bin] || 0)/this.number/this.step
    }

    this.quantileData = null
    this.quantileFun = null
    this.CDFFun = null
    this.buildQuantiles = function()
    {
	var res = this.bins
	    .map(b => this.data[b])
	    .accumsum()
	
	this.quantileData = res
	    .map(x => x/res.last())
	    .zip(this.bins.map(x => x-this.step/2).concat([this.maxBin+this.step/2]))
	this.quantileFun = linearInterpolation(this.quantileData)
	this.CDFFun = linearInterpolation(this.quantileData.map(x=>x.reversed()))
    }

    this.CDF = function(x)
    {
	if (this.quantileData == null)
	    this.buildQuantiles()
	if (x <= this.minBin) return 0
	if (x >= this.maxBin) return 1
	return this.CDFFun(x)
	//return this.quantileData.find(([q,b]) => x <= b)[0]
    }
    
    this.quantile = function(x)
    {
	if (this.quantileData == null)
	    this.buildQuantiles()
	if (x <= 0) return null
	if (x >= 1) return null
	return this.quantileFun(x)
	//var res = this.quantileData.find(([q,b]) => x <= q)
	//return res && res[1]
    }

    this.quantiles = function()
    {
	if (this.quantileData == null)
	    this.buildQuantiles()
	return this.quantileData
    }   
}

isHistogram = function (x) {
    return (typeof x == 'object') && x.isHistogram || false
}

Histogram.prototype.csumf = function(f)
// 𝛴f(xᵢ)vᵢ
{
    var res = 0,x
    var h = this.step
    var a = this.minBin
    var b = this.maxBin
    var N = ceil((b-a)/h)
    var k = i => i == 0 ? 1
	: i == N ? 1
	: i % 2 == 1 ? 4 : 2
    for(var i = 0; i <= (b-a)/h; i++) {
	x = i*h+a
    	res += k(i)*f(x)*this.data[x] || 0
    }
    return res*h/3
}

Histogram.prototype.sumf = function(f)
// 𝛴f(xᵢ)vᵢ
{
    var k = i => i==0 ? 1
	: i==this.bins.length-1 ? 1
	: i % 2 == 1 ? 4 : 2
    var h = (this.maxBin-this.minBin)/this.bins.length/this.step
    return this.bins.rsum(b=>f(b-this.step/2)*(this.PMF(b)||0))
//    return this.bins.rsum((b,i)=>k(i)*f(b-this.step/2)*(this.data[b]||0))*h/3
}


Histogram.prototype.expected = function(f)
// expectation for a given expression
{
    return this.sumf(f)/this.number
}

Histogram.prototype.zipWith = function(h,f)
{
    var d1 = this.samples(this.number)
    var d2 = h.samples(h.number)
    return new Histogram(this.step).fromList(d1.zipWith(d2,f))
}

Histogram.prototype.toList = function()
{
    m = this.minValue
    return this.bins.mapappend(b => Normal(b,this.step/4).samples(this.data[b]*m))
}

Histogram.prototype.samples = function(n)
{
    var res = [], g = this.generator
    for(var i = 0; i< n; i++)
	res.push(g())
    return res
}

Histogram.prototype.resample = function(s)
{
    var h = new Histogram(s).fromList(this.samples(this.number))
    var m = h.mean
    return h.shift(this.mean-m)
}

Histogram.prototype.map = function(f)
{
    return new Histogram(this.step)
	.fromList(this.samples(this.number).map(f))
}

Histogram.prototype.scale = function(a)
{
    return this.map(x => x*a)
}

Histogram.prototype.shift = function(a)
{
    return this.map(x => x+a)
}

Object.defineProperty
(Histogram.prototype, "support",
 { get : function ()
   {
       return [this.minBin,this.maxBin]
   }
 }
)

Object.defineProperty
(Histogram.prototype, "generator",
 { get : function ()
   {
       return () => this.quantile(Math.random())
   }
 }
)

Object.defineProperty
(Histogram.prototype, "mean",
 { get : function ()
   {
       return this.total/this.number
   }
 }
)

Object.defineProperty
(Histogram.prototype, "gmean",
 { get : function ()
   {
       return exp(this.expected(ln))
   }
 }
)


Object.defineProperty
(Histogram.prototype, "median",
 { get : function ()
   {
       return this.quantile(1/2)
   }
 }
)

Object.defineProperty
(Histogram.prototype,
 "variance",
 { get : function ()
   {
       return this.expected(b => sqr(b-this.mean))
   }
 }
)

Object.defineProperty
(Histogram.prototype,
 "stdev",
 { get : function ()
   {
	return sqrt(this.variance) 
   }
 }
)

Object.defineProperty
(Histogram.prototype,
 "LorentzCurve",
 { get : function ()
   {
       var res = [[0,0]], x = 0, y = 0
       lst = this.bins.map(b=>[this.data[b],b]).sorted((x,y)=>-x[0]+y[0])
       lst.forEach(([d,b]) => res.push([x+=d,y+=d*b*this.step]))
       return res.map(r => [r[0]/x,r[1]/y])
   }
 }
)

Object.defineProperty
(Histogram.prototype,
 "Gini",
 { get : function ()
   {
       let l = this.LorenzCurve,res = 1
       for(var i = 1;i< l.length;i++)
	   res -= (l[i][0]-l[i-1][0])*(l[i][1]+l[i-1][1])
       return res
   }
 }
)

Object.defineProperty
(Histogram.prototype, "entropy",
 { get : function ()
   {
       var res = 0
       this.bins.forEach(b => {
	   if (this.PMF(b) > 0)
	       res += this.PMF(b)*ln(this.PMF(b)*this.step)*this.step
       })
       return -res
   }
 }
)

Histogram.prototype.quantilePlot = function (data)
{
    if (Array.isArray(data)) {
	return this.quantilePlot(new Histogram(this.step).fromList(data))
    }
    if (isDistribution(data)) {
	if (data.quantile == 'not implemented')
	    return this.quantilePlot(data.samples(this.number)) 
	else
	    return [this.quantiles()
		    .map(([q,b]) => [data.quantile(q),b])
		    .filter(([a,b]) => !(a == null || a == Infinity || isNaN(a))),
		    data]
    }
    if (typeof data== 'function' && isDistribution(data())) {
	return this.quantilePlot(data().fromHistogram(this))
    }
    if (isHistogram(data)) {
	var l1 = data.quantiles().map(([q,b]) => [b,this.quantile(q)])
	var l2 = this.quantiles().map(([q,b]) => [data.quantile(q),b])
	return [l1.merge(l2).filter(([a,b]) => a != null && b != null),
		data]
    }
}

Histogram.prototype.QQPlot = function (fig,data, opts)
{
    var defaults = {
	axisType  : 'linear',
	show  : 'values',
	quantiles : [0.01,0.1,0.25,0.5,0.75,0.9,0.99] 
    }
    var options = Object.assign(defaults, opts || {})    

    var qq,f
    [qq,f] = this.quantilePlot(data)
    var qs = options.quantiles
    var res = new Graphics(fig, opts)
    if (options.axesType == 'log')
    {
	qq = qq.filter(([a,b]) => a > 0 & b > 0)
	res.xAxisType('log').yAxisType('log')
    }
    res .listPlot(qq, {'class':'QQdata'})
	.plot(x => x, {'class':'QQdiagonal'})
    	.gridLines({x : qs.map(x => f.quantile(x)),
		    y : qs.map(x=> this.quantile(x))})

    if (options.show == 'quantiles')
	res.axes({xTickValues : qs.map(x => f.quantile(x)),
		  xTickFormat : d => fmt.percent(f.CDF(d)),
		  yTickValues : qs.map(x => this.quantile(x)),
		  yTickFormat : d => fmt.percent(this.CDF(d))})
    else
	return res.axes({})
    
}

// EXAMPLE
/*
new Graphics(fig1)
    h.QQPlot(Geometric)
*/

Histogram.prototype.WeibullPlot = function (fig,opts)
{
    pts = this.bins.tail().most().map(b => [b,-ln(1-this.CDF(b))])
    var	qs = [0.01,0.1,0.25,0.5,0.75,0.9,0.99] 
    var res = new Graphics(fig, opts)
    res.xAxisType('log')
	.yAxisType('log')
	.listPlot(pts, {'class':'QQdata'})
	.axes({xTicks: 3,
	       yTicks: 3})
	.listPlot([pts.head(),pts.last()], {'joined':true,
					    'points':false,
					    'class':'QQdiagonal'})
}

////////////////////////////////////////////////////////////

function Gamma(a,b)
{
    var res = new ContinuousDistribution({
	shape : a || 1,
	rate  : b || 1
    })
    res.support   = [0,Infinity]
    res.PDF = pdf(res,function (x)
    {
	if (x <= 0) return 0
	return pow(b,a)/gamma(a)*pow(x,a-1)*exp(-b*x)
    })
    res.mean      = a / b
    res.variance  = a / sqr(b)
    res.mode      = a >= 1 ? (a - 1)/b : 0
    res.generator = gammaGen(a,b)
    res.fromHistogram  = function (h)
    {
	var s1 = h.expected(ln)
	var s = ln(h.mean) - s1,
	    a = (3 - s + sqrt((s-3)*(s-3)+24*s))/(12*s),
	    b = a/h.mean
	return Gamma(a, b)
    }
    return res
}

function gammaGen(a,b)
{
    return function ()
    {
	// Gamma(alpha,lambda) generator using Marsaglia and Tsang method
	// Algorithm 4.33
	var randn = d3.randomNormal(0,1), rand = random
	if (a>1)
	{
	    var d=a-1/3,
		c=1/sqrt(9*d),
		flag = true,
		Z,V,U
	    while (flag)
	    {
		Z = randn()
		if (Z>-1/c)
		{
		    V=pow(1 + c*Z, 3)
		    U=rand()
		    flag = ln(U) > (Z*Z/2 + d - d*V + d*ln(V))
		}
	    }
	    return d*V/b;
	}
	else
	{
	    return gammaGen(a+1,b)()*pow(random(),1/a);
	}
    }
}

////////////////////////////////////////////////////////////

function Exponential(l)
{
    var res = new ContinuousDistribution({
	rate : l || 1
    })
    res.support  = [0,Infinity]
    res.PDF      = pdf(res,x => l*exp(-x*l))
    res.CDF      = cdf(res,x => 1 - exp(-l*x))
    res.quantile = quantileFun(x => -ln(1-x)/l)
    res.mean     = 1/l
    res.median   = ln(2)/l
    res.variance = 1/l/l
    res.stdev    = 1/l
    res.mode     = 0
    res.generator = () => -ln(random())/l
    res.fromHistogram = h => Exponential(1/h.mean)
    return res
}

////////////////////////////////////////////////////////////

function Poisson(l)
{
    var res = new DiscreteDistribution({
	    rate : l || 1
    })
    res.PMF      = pdf(res,function (k) {
	var x = floor(k)
	return pow(l,x)*exp(-l)/fact(x)
    })
    res.CDF      = cdf(res,k => exp(-l)*sum(i => pow(l,i)/fact(i),0,floor(k)))
    res.support  = [0,Infinity]
    res.mean     = l
    res.median   = floor(l + 1/3 - 0.02/l)
    res.variance = l
    res.mode     = floor(l)
    res.generator = function ()
    {
	var p = 1, n = 0;
	while (p > exp(-l))
	{
	    p *= random()
	    n++
	}
	return n-1
    }
    res.fromHistogram = h => Poisson(h.mean)
    return res
}

////////////////////////////////////////////////////////////

function Binomial(n,p)
{
    var res = new DiscreteDistribution({
	trials : n || 1,
	p : p || 0.5
    })
    var q = 1 - p
    res.support  = [0,n]
    res.PMF      = pdf(res,function (k) {
	return C(n,k)*pow(p,k)*pow(q,n-k)
    })
    res.mean     = n*p
    res.median   = floor(n*p)
    res.variance = n*p*q
    res.mode     = floor((n+1)*p)
    res.generator = function ()
    {
	return ceil(res.quantile(Math.random()))-1
    }
    res.fromHistogram = function(h,opts)
    {
	if (!opts || opts === {}) return undefined
	if (opts.p) return Binomial(Math.round(h.mean/opts.p),opts.p) 
	if (opts.trials) return Binomial(opts.trials,h.mean/opts.trials) 
    }
    res.quantile  = quantileFun(q => findRoot(res.CDF,0,x => x <= q)[0])
    return res
}

////////////////////////////////////////////////////////////

function Normal(m,v)
{
    var s = sqrt(v)
    var res = new ContinuousDistribution({
	mean : m || 0,
	variance  : v || 1
    })
    res.PDF = x => sqrt(0.5/pi/v)*exp(-(x-m)*(x-m)/(2*v))
    res.CDF       = x => 1/2*(1 + erf((x-m)/(s*sqrt(2))))
    res.support   = [-Infinity,Infinity]
    res.mean      = m  
    res.median    = m 
    res.variance  = s*s 
    res.stdev     = s 
    res.mode      = m 
    res.generator = d3.randomNormal(m,s)
    res.fromHistogram = h => Normal(h.mean,h.variance)
    return res
}

////////////////////////////////////////////////////////////

function LogNormal(m,s2)
{
    var s = sqrt(s2)
    var res = new ContinuousDistribution({
	mean : m || 1,
	variance  : s || 1
    })
    res.support   = [0,Infinity]
    res.PDF       = pdf(res,x => 1/(x*s*sqrt(2*pi))*exp(-sqr(ln(x)-m)/(2*s2)))
    res.CDF       = cdf(res,x => 1/2*(1 + erf((ln(x)-m)/(s*sqrt(2)))))
    res.mean      = exp(m + s2/2)  
    res.median    = exp(m) 
    res.variance  = (exp(s2) - 1)*exp(2*m+s2)
    res.stdev     = s
    res.mode      = exp(m - s2)
    res.generator = function ()
    {
	return exp(d3.randomNormal(m,s2)())
    }
    res.fromHistogram = function (h)
    {
	var m = h.expected(ln)
	var s2 = h.expected(x => sqr(ln(x) - m))
	return LogNormal(m,s2)
    }
    return res
}

////////////////////////////////////////////////////////////

function Pareto(k,a)
{
    var res = new ContinuousDistribution({
	scale : k || 1,
	shape  : a || 1
    })
    res.support   = [k,Infinity]
    res.PDF = pdf(res,function (x)
    {
	if (x < k) return 0
	return a*pow(k,a)/pow(x,a+1)
    })
    res.CDF = cdf(res,function (x)
    {
	if (x < k) return 0
	return 1 - pow(k/x,a)
    })
    res.quantile  = quantileFun(q => k*pow(1-q,-1/a))
    res.mean      = (a <= 1) ? Infinity : a*k/(a-1)
    res.median    = k*pow(2,1/a)
    res.variance  = (a <= 2) ? Infinity : a*k*k/(sqr(a-1)*(a-2))
    res.mode      = k
    res.generator = () => k/pow(random(),1/a)
    res.samples   = n => d3.range(n).map(res.generator)
    res.fromHistogram = h => Pareto(h.minBin,1/h.expected(x => ln(x) - ln(h.minBin)))

    return res   
}

////////////////////////////////////////////////////////////

function Uniform([a,b])
{
    var res = new ContinuousDistribution({
	domain : [a,b] || [0,1]
    })
    res.support   = [a,b]
    res.PDF = pdf(res,x => 1/(b-a))
    res.CDF = cdf(res,d3.scaleLinear().range([0,1]).domain([a,b]))
    res.quantile  = quantileFun(d3.scaleLinear().range([a,b]).domain([0,1]))
    res.mean      = (b + a)/2
    res.median    = res.mean
    res.variance  = 1/12*sqr(b - a)
    res.mode      = undefined
    res.generator = () => random()*(b-a)+a
    res.fromHistogram = h => Uniform([h.minBin,h.maxBin])
    return res   
}

////////////////////////////////////////////////////////////

const DeltaDistribution = x => Uniform([x,x])

////////////////////////////////////////////////////////////

function Bernoulli(p)
{
    var res = new DiscreteDistribution({
	probability : p || 0.5
    })
    var q = 1 - p
    res.support   = [0,1]
    res.PMF = pdf(res, x => x < 1/2 ? q : p)
    res.CDF = cdf(res,x => q)
    res.mean      = p
    res.median    = (q > p) ? 0 : (q == p) ? 0.5 : 1
    res.variance  = p * q
    res.mode      = (q > p) ? 0 : (q == p) ? undefined : 1
    res.generator = () => Math.random() < p ? 0 : 1
    res.fromHistogram = h => Bernoulli(h.data[1])
    return res   
}

////////////////////////////////////////////////////////////

function Geometric(p)
{
    var res = new DiscreteDistribution({
	'p' : p || 0.5
    })
    var q = 1 - p
    res.support   = [1,Infinity]
    res.PMF = pdf(res, k => pow(q,floor(k)-1)*p)
    res.CDF = cdf(res, k => 1 - pow(q,k))
    res.quantile  = quantileFun(x => ceil(log(q,1-x)))
    res.mean      = 1/p
    res.median    = ceil(-log(q,2))
    res.variance  = q/sqr(p)
    res.mode      = 1
    res.generator = () => ceil(log(q,Math.random()))
    res.fromHistogram = h => Geometric(1/h.mean)
    return res   
}

function Geometric0(p)
{
    var res = new DiscreteDistribution({
	'p' : p || 0.5
    })
    var q = 1 - p
    res.support   = [0,Infinity]
    res.PMF = pdf(res, k => pow(q,floor(k))*p)
    res.CDF = cdf(res, k => 1 - pow(q,k+1))
    res.quantile  = quantileFun(x => ceil(log(q,1-x)))
    res.mean      = (1-p)/p
    res.median    = ceil(-log(q,2))-1
    res.variance  = q/sqr(p)
    res.mode      = 0
    res.generator = () => floor(log(q,Math.random()))
    res.fromHistogram = h => Geometric(1/(h.mean+1))
    return res   
}

////////////////////////////////////////////////////////////

function Bag(lst)
{
    this.contents = lst
    this.sample   = () => {
	if (this.contents.length == 0)
	    return null
	var i = floor(random()*this.contents.length)
	var res = this.contents[i]
	this.contents = this.contents.removeAt(i)
	return res
    }
    this.samples = n =>
    {
	if (this.contents.length == 0)
	    return null
	return d3.range(min(n,this.contents.length)).map(this.sample)
    }
}



////////////////////////////////////////////////////////////

function Process(timeD,accD,accum)
{
    var gent = timeD.generator, gena = accD.generator 
    this.t = 0
    this.acc = 0
    if (!accum)
	this.next = function () {
	    return [this.t += gent(), this.acc += gena()]
	}
    else
	this.next = function () {
	    return [this.t += gent(), this.acc = gena()]
	}

    this.get = function () {
	return [this.t, this.acc]
    }
    this.reset = function () {
	this.t = 0
	this.acc = 0
	return this
    }
    this.samples = function (n) {
	var res = [this.get()]
	repeat(n,()=>res.push(this.next()))
	return res
    }
    this.runToTime = function (T) {
	var res = [this.get()]
	while (this.t<T)
	    res.push(this.next())
	return res
    }   
    this.runToVal = function (V) {
	var res = [this.get()]
	while (this.acc<V)
	    res.push(this.next())
	return res
    }   
}


const PoissonProcess = l => new Process(Exponential(l),DeltaDistribution(1))
const GammaProcess = (a,b) => new Process(Gamma(a,b),DeltaDistribution(1))
const RandWalk = p => new Process(DeltaDistribution(1),p)
const RandProcess = p => new Process(DeltaDistribution(1),p,false)


function DiscretePoissonProcess(l)
{
    var g = Exponential(l).generator
    var T = g()
    return () => {
	if (T > 0)
	{
	    T--
	    return 0
	}
	else
	{
	    T = g()
	    return 1
	}
    }
}

function RandomWalk(opts)
{
    var g = opts && opts.generator || d3.randomNormal(0,1)
    var x = opts && opts.start || 0;
    var xmax = opts && opts.limit || Infinity
    return () => { return x = max(-xmax, min(xmax, x + g())) }
}

function dice(states)
{
    return () => states[floor(random()*states.length)]
}

function randomInteger(n)
{
    return () => floor(random()*n)
}

function cycling(distr,N)
{
    var N = N || 100
    return x => range(-N,N).sum(k=>distr.PDF(x+2*pi*k),0)
}

////////////////////////////////////////////////////////////

const Client = (i,o,n,s) => [i,o,n,s]
const tIn = fst
const tOut = snd
const cN = x => x[2]
const tServ = x => x[3]

function GG1(gi,go,e)
{
    var t = 0, q = [], n = 1
    var out = go.generator
    var newComer = gi.generator
    var newt = newComer()
    impatient = e ? Bernoulli(1-e).generator : () => false
    this.next = function () {
	while (q.length > 0 && tOut(q[0]) < t+newt) {
	    outer=q.shift()
	    return [tOut(outer), q.copy(),tOut(outer)-tIn(outer)]
	}
	t += newt
	var o = out()
	if (q.length==0)
	    q.push(Client(t,t + o,n++,o))
	else if (impatient())
	{
	    var fst = q.shift()
	    q = q.map(c => Client(tIn(c),tOut(c)+o,cN(c),tServ(c)))
	    q.unshift(Client(t, tOut(fst)+o, n++))
	    q.unshift(fst)
	}
	else
	    q.push(Client(t,tOut(q.last()) + o,n++,o))
	newt = newComer()
	return [t, q.copy(),"enqueue"]
    }
    this.reset = function () {
	t = 0
	q = []
	return this
    }
    this.samples = function (n) {
	var res = []
	repeat(n,()=>res.push(this.next()))
	return res
    }
    this.runToTime = function (T) {
	var res = []
	while (t<T)
	    res.push(this.next())
	return res
    }
    this.runToVal = function (N) {
	var res = []
	while (q.length<N)
	    res.push(this.next())
	return res
    }
}


const MG1 = (l,g,e) => new GG1(Exponential(l),g,e)
const MM1 = (l,m,e) => new GG1(Exponential(l),Exponential(m),e)
const DD1 = (l,m,e) => new GG1(DeltaDistribution(1/l),DeltaDistribution(1/m),e)
const MD1 = (l,m,e) => new GG1(Exponential(l),DeltaDistribution(1/m),e)
const DM1 = (l,m,e) => new GG1(DeltaDistribution(1/l),Exponential(m),e)

function tests1()
{
    var h = new Histogram(1).fromDistribution(DeltaDistribution(0.5),100000)
    var h1 = h.resample(1.5)
    new Graphics(fig1)
	.yRange([0,2])
	.xRange([-5,5])
	.histogram(h,{'type':'PDF','class':'queueL'})
	.histogram(h1,{'type':'PDF','class':'queueW'})
	.axes({})
}
