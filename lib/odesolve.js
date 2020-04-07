function ODEnestList(f,x,n) {
    var res = [x]
    for (var i = 1; i <= n; i++) {
	x = f.apply(f,x)
	res.push(x)
    }
    return res
}

function euler(F, h) {
    return function (t,r) {
	var n = r.length
	var res = []
	for (var i = 0; i<n; i++) res.push(r[i] + h*F(t,r)[i])
	return [t+h, res]
    }
}

function rk4(F, h) {
    return function (t,r) {
	var i, n = r.length
	var k1 = F(t,r), r1 = []
	for (i = 0; i<n; i++) r1[i] = r[i] + h/2*k1[i]
	var k2 = F(t+h/2, r1)
	for (i = 0; i<n; i++) r1[i] = r[i] + h/2*k2[i]
	var k3 = F(t+h/2, r1)
	for (i = 0; i<n; i++) r1[i] = r[i] + h*k3[i]
	var k4 = F(t+h, r1)
	var res = []
	for (i = 0; i<n; i++) res.push(r[i] + h/6*(k1[i]+2*k2[i]+2*k3[i]+k4[i]))
	return [t+h, res]
    }
}

const ODE = {
    solve: function (F, init, tspan, h, method) {
	const step = h || (tspan[1]-tspan[0])/100
	const solver = method || rk4
	const N = Math.round((tspan[1]-tspan[0])/step)
	return ODEnestList(solver(F, step), [tspan[0], init], N)
    },

    solution: function (sol,n) {
	return  (typeof n == 'number')
	    ? sol.map(x => [x[0],x[1][n]])
	    : sol.map(x => [x[1][n[0]],x[1][n[1]]])
    }
}
