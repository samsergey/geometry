const id = x => x
const pi = Math.PI
const deg = 180/pi
const sin = Math.sin
const cos = Math.cos
const exp = Math.exp
const ln = Math.log
const log2 = Math.log2
const pow = Math.pow
const sqrt = Math.sqrt
const min = Math.min
const max = Math.max
const abs = Math.abs
const floor = Math.floor
const ceil = Math.ceil
const random = Math.random
const range = d3.range
const eulerGamma = 0.577215664901
const deg2rad = x => x/180*Math.PI
const rad2deg = x => x*180/Math.PI

const add = (x,y) => x + y
const sub = (x,y) => x - y
const prod = (x,y) => x * y
const div = (x,y) => x / y
const lt = (x,y) => x < y
const lte = (x,y) => x <= y
const gt = (x,y) => x > y
const gte = (x,y) => x >= y


function log(b,x) { return Math.log(x)/Math.log(b) || Math.log(b) }

const log10 = x => log(10,x)

function sqr(x) {return x*x}

function Heaviside(x) { return x < 0 ? 0 : 1 }

function Theta(x) { return Math.atan(100*x)/Math.PI+1/2 }

function Delta(x) { return 100/(Math.PI*(1 + 10000*x*x)) }

function gamma(x) {
    var p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
             771.32342877765313, -176.61502916214059, 12.507343278686905,
             -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
	    ];
    
    var g = 7;
    if (x < 0.5) {
        return Math.PI / (Math.sin(Math.PI * x) * gamma(1 - x));
    }
    
    x -= 1;
    var a = p[0];
    var t = x + g + 0.5;
    for (var i = 1; i < p.length; i++) {
        a += p[i] / (x + i);
    }
    
    return Math.sqrt(2 * Math.PI) * Math.pow(t, x + 0.5) * Math.exp(-t) * a;
}

const digamma = derivative(x => ln(gamma(x)))

function erf(x){
    if (x < 0) return -erf(-x)
    return 2/sqrt(pi)*integrate(t => exp(-t*t),[0,x])
}

function fact(n) {
    if (n < 1) return 1
    var res = 1
    for(i=2;i<=n;i++) res*=i
    return res
}

function SimpsonIntegrate(f,a,x)
{
    var h = (x - a)/100; s = f(a)
    for(var i = a+h; i < x-h; i+=2*h)
	s += 4*f(i) + 2*f(i+h)
    s += 4*f(i)+f(x)
    return h*s/3
}

function piecewise([x1,x2],f)
{
    return x => (x < x1 || x > x2) ? 0 : f(x)
}

// step 1: a basic LUT with a few steps of Pascal's triangle
var binomials = [
    [1],
    [1,1],
    [1,2,1],
    [1,3,3,1],
    [1,4,6,4,1],
    [1,5,10,10,5,1],
    [1,6,15,20,15,6,1],
    [1,7,21,35,35,21,7,1],
    [1,8,28,54,70,54,28,8,1],
];

// step 2: a function that builds out the LUT if it needs to.
function binomial(n,k) {
    while(n >= binomials.length) {
	let s = binomials.length;
	let nextRow = [];
	nextRow[0] = 1;
	for(let i=1, prev=s-1; i<s; i++) {
            nextRow[i] = binomials[prev][i-1] + binomials[prev][i];
	}
	nextRow[s] = 1;
	binomials.push(nextRow);
    }
    return binomials[n][k];
}

C = binomial

function sum(f,x1,x2,dx)
{
    var s = 0, dx = dx || 1
    for(var x = x1; x < x2; x+=dx)
	s+=f(x)
    return s
}

function segment([x1,y1],[x2,y2])
{
    return x => (x-x1)/(x2-x1)*(y2-y1)+y1
}

function linearInterpolation(data)
{
    var n = data.length
    if (n == 0)
	console.error(`Can't interpolate empty data`)
    if (n == 1)
	return x => data[0][1]
    if (n == 2)
	return segment(data[0],data[1])
    return function (x)
    {
	if (x <= data[0][0])
	    return segment(data[0],data[1])(x)
	if (x >= data[n-1][0])
	    return segment(data[n-2],data[n-1])(x)
	var i = data.position(([z,y]) => z >= x)
	return segment(data[i-1],data[i])(x)
    }
}

function stepInterpolation(data)
{
    var n = data.length
    if (n == 0)
	console.error(`Can't interpolate empty data`)
    if (n == 1)
	return x => data[0][1]
    return function (x)
    {
	if (x <= data[0][0])
	    return data[0][1]
	if (x >= data[n-1][0])
	    return data[n-1][1]
	var i = data.position(([z,y]) => z > x)
	return x - data[i-1][0] < data[i][0] - x ? data[i-1][1] : data[i][1]
    }
}

function listIntegralInterpolation(data)
{
    var n = data.length
    if (n < 1)
	return console.error(`Not enough data`)
    return function (x)
    {
	if (x < data[0][0]) return data[0][1]
	var i = data.position(([z,y]) => z > x) || data.length
	var d = data.take(i)
	var s = d.zipWith(d.tail(),([x1,y1],[x2,y2])=>(x2-x1)*(y1+y2)/2).sum()
	var ds = i < data.length ? integrate(segment(data[i-1],data[i]),[data[i-1][0],x]) : 0
	return data[0][1] + s + ds
    }
}


function within(xmin,xmax)
{
    return x => min(xmax,max(x,xmin))
}

function BesselJ0(x) {
    var ax,z,xx,y,ans,ans1,ans2;
    ax = Math.abs(x)
    if (ax < 8.0) {
	y = x * x;
	ans1 = 57568490574.0 + y * (-13362590354.0 + y * (651619640.7 + y * (-11214424.18 + y * (77392.33017 + y * (-184.9052456)))));
	ans2 = 57568490411.0 + y * (1029532985.0 + y * (9494680.718 + y * (59272.64853 + y * (267.8532712 + y * 1.0))));
	ans = ans1 / ans2;
    } else {
	z = 8.0 / ax;
	y = z * z;
	xx = ax - 0.785398164;
	ans1 = 1.0 + y * (-0.1098628627e-2 + y * (0.2734510407e-4 + y * (-0.2073370639e-5 + y * 0.2093887211e-6)));
	ans2 = -0.1562499995e-1 + y * (0.1430488765e-3 + y * (-0.6911147651e-5 + y * (0.7621095161e-6 - y * 0.934935152e-7)));
	ans = Math.sqrt(0.636619772 / ax) * (Math.cos(xx) * ans1 - z * Math.sin(xx) * ans2);
    }
    return ans;
}

function BesselJ1(x) {
    var ax,z,xx,y,ans,ans1,ans2;
    ax = Math.abs(x);
    if (ax < 8.0) {
	y=x*x;
	ans1 = x*(72362614232.0+y*(-7895059235.0+y*(242396853.1+y*(-2972611.439+y*(15704.48260+y*(-30.16036606))))));
	ans2 = 144725228442.0+y*(2300535178.0+y*(18583304.74+y*(99447.43394+y*(376.9991397+y*1.0))));
	ans = ans1/ans2;
    } else {
	z=8.0/ax;
	y=z*z;
	xx=ax-2.356194491;
	ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4+y*(0.2457520174e-5+y*(-0.240337019e-6))));
	ans2=0.04687499995+y*(-0.2002690873e-3+y*(0.8449199096e-5+y*(-0.88228987e-6+y*0.105787412e-6)));
	ans=Math.sqrt(0.636619772/ax)*(Math.cos(xx)*ans1-z*Math.sin(xx)*ans2);
	if (x < 0.0) ans = -ans;
    }
    return ans;	
}

function BesselY0(x) {
    var z, xx, y, ans, ans1, ans2;
    if (x < 8.0)  {
	y=x*x;
	ans1 = -2957821389.0+y*(7062834065.0+y*(-512359803.6+y*(10879881.29+y*(-86327.92757+y*228.4622733))));
	ans2 = 40076544269.0+y*(745249964.8+y*(7189466.438+y*(47447.26470+y*(226.1030244+y*1.0))));
	ans = (ans1/ans2)+0.636619772*BesselJ0(x)*Math.log(x);
    } else {
	z=8.0/x;
	y=z*z;
	xx=x-0.785398164;
	ans1 = 1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4+y*(-0.2073370639e-5+y*0.2093887211e-6)));
	ans2 = -0.1562499995e-1+y*(0.1430488765e-3+y*(-0.6911147651e-5+y*(0.7621095161e-6+y*(-0.934945152e-7))));
	ans = Math.sqrt(0.636619772/x)*(Math.sin(xx)+ans1+z*Math.cos(xx)*ans2);
    }
    return ans;
}

function BesselY1(x) {
    var z, xx, y, ans, ans1, ans2;
    if (x < 8.0) { 
	y=x*x
	ans1 = x*(-0.4900604943e13+y*(0.1275274390e13+y*(-0.5153438139e11+y*(0.7349264551e9+y*(-0.4237922726e7+y*0.8511937935e4)))));
	ans2 = 0.2499580570e14+y*(0.4244419664e12+y*(0.3733650367e10+y*(0.2245904002e8+y*(0.1020426050e6+y*(0.3549632885e3+y)))));
	ans = (ans1/ans2)+0.636619772*(BesselJ1(x)*Math.log(x)-1.0/x);
    } else {
	z=8.0/x;
	y=z*z;
	xx=x-2.356194491;
	ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4+y*(0.2457520174e-5+y*(-0.240337019e-6))));
	ans2=0.04687499995+y*(-0.202690873e-3+y*(0.8449199096e-5+y*(-0.88228987e-6+y*0.10578e-6)));
	ans=Math.sqrt(0.636619772/x)*(Math.sin(xx)*ans1+z*Math.cos(xx)*ans2);
    }
    return ans;
}

function BesselI0(x) {
    var ax,ans,y;
    ax=Math.abs(x);
    if (ax < 3.75) {
	y=x/3.75;
	y*=y;
	ans=1.0+y*(3.5156229+y*(3.0899424+y*(1.2067492+y*(0.2659732+y*(0.360768e-1+y*0.45813e-2)))));
    } else {
	y=3.75/ax;
	ans=(Math.exp(ax)/Math.sqrt(ax))*(0.39894228+y*(0.1328592e-1+y*(0.225319e-2+y*(-0.157565e-2+y*(0.916281e-2+y*(-0.2057706e-1+y*(0.2635537e-1+y*(-0.1647633e-1+y*0.392377e-2))))))));
    }
    return ans;
}

function BesselI1(x) {
    var ax,ans,y;
    ax=Math.abs(x);
    if (ax<3.75) {
	y=x/3.75;
	y*=y;
	ans=ax*(0.5+y*(0.87890594+y*(0.51498869+y*(0.15084934+y*(0.2658733e-1+y*(0.301532e-2+y*0.32411e-3))))));
    } else {
	y=3.75/ax;
	ans=0.2282967e-1+y*(-0.2895312e-1+y*(0.1787654e-1-y*0.420059e-2));
	ans=0.39894228+y*(-0.3988024e-1+y*(-0.362018e-2+y*(0.163801e-2+y*(-0.1031555e-1+y*ans))));
	ans*=(Math.exp(ax)/Math.sqrt(ax));
    }
    return x < 0.0 ? -ans : ans;
}

function BesselK0(x) {
    var y,ans;
    if (x <= 2.0) {
	y=x*x/4.0;
	ans=(-Math.log(x/2.0)*BesselI0(x))-0.57721566+0.42278420*y+0.23069756*y*y+0.03488590*y*y*y+0.00262698*y*y*y*y+0.00010750*y*y*y*y*y+0.00000740*y*y*y*y*y*y;
    } else {
	y=2.0/x;
	ans=(Math.exp(-x)/Math.sqrt(x))*(1.25331414+y*(-0.7832358e-1+y*(0.2189568e-1+y*(-0.1062446e-1+y*(0.587872e-2+y*(-0.251540e-2+y*0.53208e-3))))));
    }
    return ans;
}

function BesselK1(x) {
    var y,ans;
    if (x <= 2.0) {
	y=x*x/4.0;
	ans=(Math.log(x/2.0)*BesselI1(x))+(1.0/x)*(1.0+y*(0.15443144+y*(-0.67278579+y*(-0.18156897+y*(-0.1919402e-1+y*(-0.110404e-2+y*(-0.4686e-4)))))));
    } else {
	y=2.0/x
	ans=(Math.exp(-x)/Math.sqrt(x))*(1.25331414+y*(0.23498619+y*(-0.3655620e-1+y*(0.1504268e-1+y*(-0.780353e-2+y*(0.325614e-2+y*(-0.68245e-3)))))));
    }
    return ans;
}

function BesselJ(n,x) {
    if (n == null || n.length == 0) return "n not specified";
    if (Math.floor(n) != n) return "Error: n not integer";
    if (n < 2) return "Error: n < 2";
    var ACC = 40.0;		// Make larger to increase accuracy.
    var BIGNO = 1.0e10;
    var BIGNI = 1.0e-10;
    var j,jsum,m,ax,bj,bjm,bjp,sum,tox,ans;
    ax=Math.abs(x);
    if (ax == 0.0) return 0.0;
    else if (ax > n) {
	tox = 2.0/ax;
	bjm=BesselJ0(ax);
	bj=BesselJ1(ax);
	for (j=1;j<n;j++) {
	    bjp=j*tox*bj-bjm;
	    bjm=bj;
	    bj=bjp;
	}
	ans=bj;
    } else {
	tox=2.0/ax;
	if (Math.sqrt(ACC*n) >= 0)
	    m=2*((n + Math.floor(Math.sqrt(ACC*n))) / 2);
	else
	    m=2*((n + Math.ceil(Math.sqrt(ACC*n))) / 2);
	jsum=0;
	bjp=ans=sum=0.0;
	bj=1.0;
	for (j=m;j>0;j--) {
	    bjm=j*tox*bj-bjp;
	    bjp=bj;
	    bj=bjm;
	    if (Math.abs(bj) > BIGNO) {
		bj *= BIGNI;
		bjp *= BIGNI;
		ans *= BIGNI;
		sum *= BIGNI;
	    }
	    if (jsum) sum += bj;
	    jsum=!jsum;
	    if (j == n) ans=bjp;
	}
	sum=2.0*sum-bj;
	ans /= sum;
    }
    return x < 0.0 && (n & 1) ? -ans : ans;
}

function BesselY(n,x) {
    if (n == null || n.length == 0) return "n not specified";
    if (Math.floor(n) != n) return "Error: n not integer";
    if (n < 2) return "Error: n < 2";
    var j,by,bym,byp,tox;
    tox=2.0/x;
    by=BesselY1(x);
    bym=BesselY0(x);
    for (j=1;j<n;j++) {
	byp=j*tox*by-bym;
	bym=by;
	by=byp;
    }
    return by;
}

function BesselI(n,x) {
    if (n == 0) return BesselI0(x)
    if (n == 1) return BesselI1(x)
    if (n == null || n.length == 0) return "n not specified";
    if (Math.floor(n) != n) return "Error: n not integer";
    
    var ACC = 40.0;		// Make larger to increase accuracy.
    var BIGNO = 1.0e10;
    var BIGNI = 1.0e-10;
    var j,bi,bim,bip,tox,ans;
    if (x == 0.0) return 0.0;
    else {
	tox=2.0/Math.abs(x);
	bip=ans=0.0;
	bi=1.0;
        for (j=2*(n+Math.floor(Math.sqrt(ACC*n)));j>0;j--) {
	    bim=bip+j*tox*bi;
	    bip=bi;
	    bi=bim;
	    if (Math.abs(bi) > BIGNO) {
		ans *= BIGNI;
		bi *= BIGNI;
		bip *= BIGNI;
	    }
	    if (j == n) ans=bip;
	}
	ans *= BesselI0(x) / bi;
	return x < 0.0 && (n & 1) ? -ans : ans;
    }
}

function BesselK(n,x) {
    if (n == null || n.length == 0) return "n not specified";
    if (Math.floor(n) != n) return "Error: n not integer";
    if (n < 2) return "Error: n < 2";
    var j,bk,bkm,bkp,tox;
    tox=2.0/x;
    bkm=BesselK0(x);
    bk=BesselK1(x);
    for (j=1;j<n;j++) {
	bkp=bkm+j*tox*bk;
	bkm=bk;
	bk=bkp;
	0}
    return bk;
}

function listIntegralInterpolationInv(data)
{
    var dd = data.map(([x,y]) => [y,x])
    
    var n = dd.length
    if (n < 1)
	return console.error(`Not enough data`)
    return function (y)
    {
	if (y < dd[0][0]) return dd[0][1]
	var i = dd.position(([z,y]) => z > y) || data.length
	var d = dd.take(i)
	var s = d.zipWith(d.tail(),(p1,p2)=>interp(p1,p2)(p2[0])).sum()
	var ds = i < dd.length ? interp(dd[i-1],dd[i])(y) : 0
	return dd[0][1] + s + ds
    }
}

function interp([x1,y1],[x2,y2])
{
    return Y => -(sqrt(2)*x2*sqrt((Y*(y1 - y2))/(x1 - x2)) + x1*(-y1 + sqrt(2)*sqrt((Y*(y1 - y2))/(x1 - x2)) + y2))/(y1 - y2)
}
