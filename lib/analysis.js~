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
    var cond = cond || (f => f == 0)
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
    cond = cond || (f => f == 0)
    eps = eps || epsilon
    return search(x0,x0 + 10*eps) || search(x0,x0-10*eps)
    function search(a,b)
    {
	var d = b - a
	return abs(d) <= 1e4 &&
	    (bisection(f,a,b,cond,eps) || search(b,b+2*d))
    }
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

function integrate(f,[a,b],d,eps,i0)
{
    var d = d || 1
    var eps = eps || epsilon
    var c = (a + b) / 2
    var i0 = i0 || gaussIntegrate(f,[a,b])
    var il = gaussIntegrate(f,[a,c])
    var ir = gaussIntegrate(f,[c,b])
    var i1 = ir + il 
    if (d > 10) return i1
    return almostEqual(i0,i1,eps) ? i1 : integrate(f,[a,c],d+1,eps,il) + integrate(f,[c,b],d+1,eps,ir)
}
