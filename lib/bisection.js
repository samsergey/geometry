// "strict mode"
function bisection(f, a, b, cond, eps) {
    cond = cond || (f => f == 0)
    eps = eps || 1e-14
    function loop(a, b, fa, fb) {
	if (!(cond(fa) ^ cond(fb))) return false
	var c = (a+b)/2, fc = f(c)
	if (Math.abs((b - a)/c) < eps || Math.abs(b-a) < eps)
	    return cond(fa) ?  [b,fb]
	    : cond(fb) ? [a,fa]
	    : [c,fc]
	else
	    return loop(a,c,fa,fc) || loop(c,b,fc,fb)
    }
    return loop(a,b,f(a),f(b))
}

