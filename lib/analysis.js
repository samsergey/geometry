var epsilon = 1e-8

function derivative(f) {
    var dx = 1e-5
    return x => (f(x+dx)-f(x-dx))/(2*dx)
}

function almostEqual(a,b,eps)
{
    return abs(b - a) < (eps || epsilon)*abs(a + b)
}

function newton(f,df,x0,d)
{
    return fixedPoint(x => x - f(x)/df(x), x0, d || 10 , id, almostEqual)
}

function bisection(f, a, b, cond, eps, d) {
    // total robust root-finder
    var cond = cond || (f => f > 0)
    var eps = eps || epsilon
    function loop(a, b, fa, fb, d) {
	if (!(cond(fa) ^ cond(fb)) || d <= 0) return false
	var c = (a+b)/2, fc = f(c)
	if (almostEqual(a,b))
	    return cond(fa) ?  [b,fb]
	    : cond(fb) ? [a,fa]
	    : [c,fc]
	else
	    return loop(a,c,fa,fc,d-1) || loop(c,b,fc,fb,d-1)
    }
    return loop(a,b,f(a),f(b),d||100)
}

function findRoot(f,x0,cond,eps,d)
{
    // total universal root-finder
    cond = cond || (f => f > 0)
    eps = eps || epsilon
    return search(x0,x0 + 10*eps) || search(x0,x0-10*eps)
    function search(a,b)
    {
	var d = b - a
	return abs(d) <= 1e4 &&
	    (bisectgion(f,a,b,cond,eps) || search(b,b+2*d))
    }
}

function secant(f,a,b,eps)
{
    var x1 = a, y1=f(x1), x2 = b, y2=f(x2),e = eps||1e-14
    while (abs(y2-y1) >= e || abs(x1-x2) >= abs(x1+x2)*e || abs(x1-x2) >= e)
    {
	[x1,y1,x2] = [x2, y2, (x1*y2-x2*y1)/(y2-y1)]
	y2 = f(x2)
    }
    return x2 
}

var Integration = {
    xs : [-1/3*Math.sqrt(5 + 2*Math.sqrt(10/7)),
	  -1/3*Math.sqrt(5 - 2*Math.sqrt(10/7)),
	  0,
	  1/3*Math.sqrt(5 - 2*Math.sqrt(10/7)),
	  1/3*Math.sqrt(5 + 2*Math.sqrt(10/7))],
    ws : [(322-13*Math.sqrt(70))/900,
	  (322+13*Math.sqrt(70))/900,
	  128/225,
	  (322+13*Math.sqrt(70))/900,
	  (322-13*Math.sqrt(70))/900]
}

function gaussIntegrate(f,[a,b])
{
    var d = (b - a) / 2, m = (b + a) / 2
    return d*Integration.xs.map(x => f(d*x + m)).dot(Integration.ws)
}

function integrate(f,[a,b],o)
{
    var opts = o || {}
    var d = opts['divisions'] || 1
    var maxd = opts['maxDivisions'] || 10
    var eps = opts['epsilon'] || epsilon

    function go(a,b,d,i0) {
	var c = (a + b) / 2
	var il = gaussIntegrate(f,[a,c])
	var ir = gaussIntegrate(f,[c,b])
	var i1 = ir + il 
	if (d > maxd) return i1
	return almostEqual(i0,i1,eps) ? i1 : go(a,c,d+1,il) + go(c,b,d+1,ir)
    }
    
    return go(a,b,gaussIntegrate(f,[a,b]))

}

function goldenMeanOptimization(f, x1, x2) {
    let goldenMean = (Math.sqrt(5)-1)/2,
	a = x1, fa = f(a),
	b = x2, fb = f(b),
	c = b - (b-a)*goldenMean, fc = f(c),
	d = a + (b-a)*goldenMean, fd = f(d)
    function go(a,c,d,b,fa,fc,fd,fb,i) {
	if (Math.abs(d - c) < 1e-8 || i > 1000)
	    return {minimum:(d+c)/2, value:f((d+c)/2)}
	if (fc < fd)
	    return go (a, a + (d - c), c, d, fa, f(a + (d - c)), fc, fd, i+1)
	else
	    return go (c, d, b - (d - c), b, fc, fd, f(b - (d - c)), fb, i+1)
	}
    return go (a,c,d,b,fa,fc,fd,fb,0)
}

function findMinimum(f, x1, x2, n = 10) {
    let pts = range(0,n+1).map(i => (x2-x1)*i/n+x1),
	mins = pts.zipWith(pts.tail(),(a,b) => goldenMeanOptimization(f,a,b))
    return mins.minBy(r => r.value)
}
