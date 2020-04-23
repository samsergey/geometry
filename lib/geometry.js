const paperSize = 50
const xRange = [-paperSize/2,paperSize/2]
const yRange = [-paperSize/2,paperSize/2]

const style = {
    svg : {
	'background':'#333'
    },

    point : {
	'stroke':'#333',
	'stroke-width':1,
	'fill':'red'
    },

    line : {
	'stroke':'orange',
	'stroke-width':2,
	'fill':'none'
    },
    
    polygon : {
	'stroke':'orange',
	'stroke-width':2,
	'fill':'none'
    },

    circle : {
	'stroke':'orange',
	'stroke-width':2,
	'fill':'none'
    },
    
    angle : {
	'stroke':'wheat',
	'stroke-width':1.5,
	'fill':'none'
    },
    
    label : {
	'font-family' : "'CMU Serif', serif",
	'font-style' : 'italic',
	'font-size' : '18px',
	'fill':'wheat',
    },
    
    text : {
	'font-family' : "'CMU Sans Serif', sans-serif",
	'font-style' : 'italic',
	'font-size' : '18px',
	'fill':'wheat',
    }
}

////////////////////////////////////////////////////////////
// math

function angleV(vector) {
    // tested
    const x = vector[0], y = vector[1]
    if (x == 0 && y == 0) return 0
    if (x == 1 && y == 0) return 0
    if (x == 0 && y == 1) return 90
    if (x == -1 && y == 0) return 180
    if (x == 0 && y == -1) return 270
    const a = rad2deg(Math.atan(y/x))
    if (x >= 0 && y >= 0) return a
    if (x <= 0 && y >= 0) return 180 + a
    if (x <= 0 && y < 0) return 180 + a
    if (x > 0 && y <= 0) return 360 + a
    return a
}

function lineEquation([x1, y1], [x2, y2], scale = 1) {
    // tested
    const v = [x2 - x1, y2 - y1]
    return t => point([x1, y1].vadd(v.scale(t*scale)))
}

function intersectionV([x1, y1], [v1x, v1y], [x2, y2], [v2x, v2y]) {
    // tested
    const D = v1y*v2x - v1x*v2y,
	  D1 = v1x*y1 - v1y*x1,
	  D2 = v2y*x2 - v2x*y2
    if (D == 0) return [1/0, 1/0]
    return [-(v1x*D2 + v2x*D1)/D, -(v1y*D2 + v2y*D1)/D]
}

function cross([x1, y1], [x2, y2]) {
    return x1*y2 - x2*y1
}

function rot90([x, y]) { return [-y, x] }

function naturalParametrization(pts, type = 'infinite') {
    if (pts.length == 0 || !type)
	return {'eqn': undefined, length : undefined}
    
    let ls, xs, ys, res
    if (type == 'closed') {
	ls = pts.rotatel().zipWith(pts, (a,b) => a.vsub(b).norm()).accumsum();
	[xs,ys] = pts.concat([pts[0]]).transpose()
    }
    else {
	ls = pts.tail().zipWith(pts, (a,b) => a.vsub(b).norm()).accumsum();
	[xs,ys] = pts.transpose()
    }
    let l = ls.last()
    let sls = ls.scale(1/l)
    let X = linearInterpolation([sls, xs].transpose())
    let Y = linearInterpolation([sls, ys].transpose())

    if (type == 'closed')
	res = s => [X(mod(s, 1)), Y(mod(s, 1))]
    else
	res = s => [X(s), Y(s)]

    return {'eqn': res, 'length': l, pointLocations: sls }
}

function differential (f) {
    const ds = 1e-8
    return s => f(s+ds).vsub(f(s-ds)).scale(1/(2*ds))
}

function vector (point1, point2) {
    return point2.xy.vsub(point1.xy)
}

const augment = ([x, y]) => [x, y, 1] 
const pure = ([x, y, _]) => [x, y] 
const trans = (T, v)  => pure(vmul(T, augment(v))) 
const rotT  = a => [[cos(a), -sin(a), 0], [sin(a), cos(a), 0], [0, 0, 1]]
const reflectT = a => [[cos(2*a), sin(2*a), 0], [sin(2*a), -cos(2*a), 0], [0, 0, 1]] 
const translateT = ([dx, dy]) => [[1, 0, dx], [0, 1, dy], [0, 0, 1]] 
const scaleT  = (a, b) => [[a, 0, 0], [0, b, 0], [0, 0, 1]] 

// Box

function appendBox (box1, box2) {
    return [[min(box1[0][0], box2[0][0]), min(box1[0][1],box2[0][1])]
	    ,[max(box1[1][0], box2[1][0]), max(box1[1][1],box2[1][1])]]
}

const Box = {mappend: appendBox,
	     mempty: [[Infinity,Infinity],[-Infinity,-Infinity]]}


////////////////////////////////////////////////////////////
// Figure and transformations

class Figure {
    constructor (label) {
	this.options = {
	    'label' : label
	}
    }

    copy (obj = null) {
	if (!obj) {
	    let res = new Figure()
	    Object.assign(res.options, this.options)
	    return res
	} else {
	    Object.assign(this.options, obj.options)
	    return this
	}
    }

    isEqual (obj) { return this == obj }

    isSimilar (obj) { return this.isEqual(obj) }
    
    transform (T) {
	console.error("transform is unimplemented")
    }

    translate(vector) {
	// tested Point
	return this.transform(translateT(vector))
    }
    
    scale(xscale, yscale = xscale) {
	// tested Point
	return this.transform(scaleT(xscale, yscale))
    }

    scaleAt(pnt, xscale, yscale = xscale) {
	// tested Point
	const pt = pnt.xy
	return this.translate(pt.flip()).scale(xscale, yscale).translate(pt)
    }

    stretch(line, scale) {
	// tested Point
	return this.rotate(-line.angle(0)).scale(1,scale).rotate(line.angle(0))
    }

    stretchAt(point, line, scale) {
	// tested Point
	const pt = point.xy
	return this.translate(pt.flip()).stretch(line, scale).translate(pt)
    }

    superpose(pnt1, pnt2) {
	// tested Point
	return this.translate(line(pnt1, pnt2).vector)
    }

    align(line1, line2) {
	// tested Point
	return this.rotateAt(line1.point(0), line2.angle(0) - line1.angle(0))
    }

    rotate(ang) {
	// tested Point
	return this.transform(rotT(deg2rad(ang)))
    }

    rotateAt(pnt, ang) {
	// tested Point
	console.assert(pnt instanceof Point,'rotation could be done against a Point')

	const pt = pnt.xy
	return this.translate(pt.flip()).rotate(ang).translate(pt)
	return this
    }

    reflect (ang) {
	// tested Point
	return this.transform(reflectT(deg2rad(ang)))
    }
    
    reflectAt(point_or_line) {
	// tested Point
	const that = point_or_line

	console.assert(that instanceof Point || that instanceof Line,
		       'reflection could be done against Point or Line')

	if (that instanceof Point) {
	    return this.scaleAt(that,-1)
	}
	if (that instanceof Line) {
	    const b = that.angle(0),
		  pt = that.pivot
	    return this.translate(pt.flip()).reflect(b).translate(pt)
	}	
	return this
    }

    option (opt, x = null) {
	if (x === null) {
	    return this.options[opt]
	} else {
	    this.options[opt] = x
	    return this
	}
    }
    
    label (l = null) {
	if (l === null )
	    return this.option('label')
	else
	    return this.option('label', l)
    }

    get isLabeled () { return !!this.options['label'] }

    lineStyle (opts) {
	const res = this.copy(),
	      st = Object.assign({}, res.options['line-style']),
	      ls = Object.assign(st,opts)
	return res.option('line-style', ls)
    }

    pointStyle (attr, value) {
	const res = this.copy(),
	      st = Object.assign({}, res.options['point-style']),
	      s = {}
	s[attr] = value
	const ls = Object.assign(st,s)
	return res.option('point-style', ls)
    }

    dashed () {	return this.lineStyle({'stroke-dasharray':'5,5'}) }
    dotted () {	return this.lineStyle({'stroke-dasharray':'2,4'}) }
    thin (th = 1) { return this.lineStyle({'stroke-width':th}) }
    color (c) {	return this.lineStyle({'stroke':c}) }
}

///////////////////////////////////////////////////////////
// Boxed
class BoxedFigure extends Figure {
    constructor (label) {
	super(label)
    }
    
    get box () { return Box.mempty }
    
    get boxWidth () {
	return 	this.boxCorner(-1,-1).distance(this.boxCorner(1,-1))
    }
    
    get boxHeight () {
	return 	this.boxCorner(-1,-1).distance(this.boxCorner(-1,1))
    }
    
    boxCorner ([dx, dy]) {
	let xmin,ymin,xmax,ymax;
	[[xmin,ymin],[xmax,ymax]] = this.box
	const xs = [xmin, (xmin+xmax)/2, xmax],
	      ys = [ymin, (ymin+ymax)/2, ymax]
	return point([xs[1+Math.sign(dx)], ys[1+Math.sign(dy)]])
    }  
    
    boxAlign (bc1, obj, bc2, space = [0,0]) {
	return new Group([
	    this,
	    obj.superpose(obj.boxCorner(bc1), this.boxCorner(bc2))
		.translate(space)
	])
    }

    beside(obj, alignment = 0, space = 0) {
	return this.boxAlign([-1, alignment], obj, [1, alignment], [space, 0]) 
    }
    below(obj, alignment = 0, space = 0) {
	return this.boxAlign([alignment, -1], obj, [alignment, 1], [0, space]) 
    }
    above(obj, alignment = 0, space = 0) {
	return this.boxAlign([alignment, 1], obj, [alignment, -1], [0, -space]) 
    }

}

class EmptyFigure extends BoxedFigure {
    constructor () { super() }
    show (paper) { }
    copy () { return this } 
    get box () { return Box.mempty }
    option () { return this }
}

///////////////////////////////////////////////////////////
// Group

class Group extends BoxedFigure {
    constructor(figures) {
	super()
	this.contents = figures
    }

    copy () {
	const res = new Group()
	res.contents = this.contents.map(f => f.copy())
	return res
    }

    transform (T) {
	const res = this.copy()
	res.contents = res.contents.map(f => f.transform(T))
	return res
    }

    element (i) {
	// tested
	return this.contents[(i - 1) % this.contents.length]
    }

    static row(figures, alignment = 0, space = 0) {
	return figures.foldr((el,res) => el.beside(res, alignment, space))
    }

    static column(figures, alignment = 0, space = 0) {
	return figures.foldr((el,res) => el.above(res, alignment, space))
    }
    
    get box () {
	return this.contents.foldMap(f => f.box, Box)
    }
    
    show (paper)
    {
	// tested
	this.contents.forEach(f => f.show(paper))
    }
}

////////////////////////////////////////////////////////////
// Point

class Point extends BoxedFigure {
    constructor (label)
    {
	super(label)
	const defaults = {
	    'labelPosition': [0, 0],
	    'labelOffset': [0, 1],
	    'pointSize' : 3, 
	    'point-style': style.point,
	    'invisible' : false
	}
	this.options = Object.assign(defaults, this.options)
	this.xy = [0, 0]
    }

    isEqual (obj) {
	// tested
	return (obj instanceof Point) &&
	    equal(this.xy, obj.xy)
    }

    copy () {
	// tested
	const res = new Point()
	Object.assign(res.options, this.options)
	res.xy = this.xy.copy()
	return res
    }
    
    transform (T) {
	// tested
	const res = this.copy()
	res.xy = trans(T, res.xy)
	return res
    }

    get x () { return this.xy[0] }
    get y () { return this.xy[1] }

    get isVisible () {
	// tested
	return plane.isEnclosing(this)
    }
    
    at (xy) {
	// tested
	const res = this.copy()
	res.xy = xy
	return res
    }
    
    on (curve, t) {
	// tested
	console.assert(curve instanceof Line || curve instanceof Circle,
		       'Point could be found on a Line or a Circle.')
	
	return this.at(curve.point(t).xy)
    }

    between (point1, point2, t = 1/2) {
	// tested
	return this.on(new Line().joining(point1, point2), t)
    }

    intersection (curve1, curve2, n = 0) {
	// tests on curves
	console.assert(curve1 instanceof Curve && curve2 instanceof Curve,
		       'intersection of non vectors')

	return this.at(curve1.intersections(curve2)[n].xy)
    }

    azimuth (point1, angle1, point2, angle2) {
	// tested
	const s = new Line().joining(point1, point2)
	const s1 = new Line().through(point1).atAngle(s.angle(0) + angle1)
	const s2 = new Line().through(point2).atAngle(s.angle(0) + 180 - angle2)
	if (s1.isParallelTo(s2))
	    return this.at([1/0,1/0])
	else
	    return this.at(s1.intersections(s2)[0].xy)
    }
    
    distance (obj) {
	// tested
	if (obj instanceof Point)
	    return this.xy.vsub(obj.xy).norm()
	if (obj instanceof Line)
	    return new Segment().through(this).heightTo(obj).length
	if (obj instanceof Circle)
	    return abs(this.distance(obj.center)-obj.R)
	return NaN
    }
    
    labelOffset (offset) {
	return this.option('labelOffset', offset.normalize().scale(1.2))
    }

    get box () { return [this.xy, this.xy] }

    invisible (f = true) {
	return this.option('invisible', f)
    }
    
    show (paper) {
	if (!(plane.isEnclosing(this))) return
	if (!this.options.invisible)
	    paper.listPlot([this.xy], this.options)
	
	if (this.isLabeled) {
	    const l = this.label()
	    const lp = this.xy.vadd([-0.3*l.length, -0.5])
	    mkLabel(paper, l, lp.vadd(this.option('labelOffset')))
	}	
    }
    
}

////////////////////////////////////////////////////////////
// Curve
class Curve extends BoxedFigure {
    constructor (label) {
	super(label)
	this.type = undefined
	this.point = undefined
	this.equation = undefined
	this.length = undefined
    }

    // ------------------------------------------------------------
    // Figure implementation


    //------------------------------------------------------
    resetEquations (s) {
	// sets parametrization, equation and length
	console.error('resetEquations is not implemented')
    }

    location (point) {
	console.error('location is not implemented')
    }

    intersections (obj) {
	console.assert(obj instanceof Curve,
		       "Intersections could be found only for curves.")
	return undefined
    }

    flip() {
    	console.error('flip is not implemented')
    }
    
    //------------------------------------------------------

    closed () {
	let res = this.copy()
	res.type = 'closed'
	res.resetEquations()
	return res
    }

    tangentV (s) {
	var df = differential(t => this.point(t).xy)
	return df(s).normalize()
    }

    tangent (s) { return new Line().through(this.point(s), this.tangentV(s)) }
    
    normalV (s) { return rot90(this.tangentV(s).normalize()) }

    normal (s) { return new Line().through(this.point(s), this.normalV(s)) }
    
    angle (s) { return angleV(this.tangentV(s)) }

    isContaining (point) { return this.equation(point.xy) }

    isEnclosing (point) {
	return ray(point).intersections(this).length % 2 == 1
    }

    isIntersecting (obj) {
	return obj instanceof Curve
	    && this.intersections(obj)
	    .some(p => this.isContaining(p) && obj.isContaining(p))
    }

    get isClosed() { return this.type == 'closed' }
    get isFinite() { return this.type == 'finite' }

    pointClosestTo (obj){
	return this.point(findMinimum(x => obj.distance(this.point(x)),0,1).minimum)
    }
}

////////////////////////////////////////////////////////////
// Line

class Line extends Curve {
    constructor(label) {
	super(label)
	const defaults = {
	    'points': false,
	    'arrow' : false, 
	    'arrowPosition' : 1, 
	    'point-style': style.point,
	    'line-style': style.line,
	    'labelPosition': 1/2,
	    'labelOffset' : 1.25
	}
	this.options = Object.assign(defaults, this.options)
	this.type = 'infinite'
	this.pivot = [0, 0]
	this.vector = [1, 0]
	this.length = Infinity
	this.resetEquations()
    }
    
    // ------------------------------------------------------------
    // Figure implementation
    copy () {
	// tested
	const res = new Line()
	Object.assign(res.options, this.options)
	res.pivot = this.pivot.copy()
	res.vector = this.vector.copy()
	res.type = this.type
	res.resetEquations()
	return res
    }
    
    transform (T) {
	// tested
	const p1 = this.point(0).transform(T),
	      p2 = this.point(1).transform(T)
	return this.joining(p1, p2)
    }

    isEqual (that) {
	// tested
	return (that instanceof Line) &&
	    equal(this.pivot, that.pivot) &&
	    equal(this.vector, that.vector)
    }

    isSimilar (obj) {
	// tested
	return (obj instanceof Line) &&
	    this.isContaining(obj.point(0)) &&
	    this.isContaining(obj.point(1))
    }

    // ------------------------------------------------------------
    // Curve implementation
    resetEquations () {
	let x0,y0,vx,vy;
	[x0,y0] = this.pivot;
	[vx,vy] = this.vector;
	this.point = s => point([x0 + s * vx, y0 + s * vy])
	this.equation = ([x,y]) => equal(vy*(x - x0), vx*(y - y0))
	return this
    }

    tangentV (t) {
	//tested 
	return this.vector.normalize()
    }

    location (point) {
	//tested
	var v = point.xy.vsub(this.pivot)
	return v.dot(this.vector)/this.vector.norm()**2
    }

    isContaining (point) {
	return equal(cross(this.vector, point.xy.vsub(this.pivot)), 0)
    }
    
    intersectionL (line) {
	if (equal(this.pivot, line.pivot))
	    return [point(this.pivot)]
	if (this.isParallelTo(line))
	    return []
	return [point(intersectionV(this.pivot, this.vector,
				    line.pivot, line.vector))]
    }

    intersections (obj) {
	console.assert(obj instanceof Curve,
		       "Intersections could be found only for curves.")
	let res = [];
	if (obj instanceof Line)
	    res = this.intersectionL(obj)
	if (obj instanceof Circle)
	    res = obj.intersectionL(this)
	if (obj instanceof Polygon)
	    res = obj.intersections(this)
	return res.filter(p => obj.isContaining(p) && this.isContaining(p))
	    .sorted((p1,p2) => this.location(p1) - this.location(p2))
    }
    
    flip () { return this.reflectAt(this.point(0)) }
    
    // ------------------------------------------------------------
    // Line methods

    // main constructor
    joining(point1, point2) {
	const res = this.copy(),
	      v = vector(point1, point2)
	res.pivot = point1.xy
	res.vector = v
	res.resetEquations()
	return res
    }

    through (point, v = this.vector) {
	const res = this.copy()
	res.pivot = point.xy
	res.vector = v
	res.resetEquations()
	return res
    }
    
    atAngle(angle_degrees) {
	// tested
	const res = this.copy()
	const r = deg2rad(angle_degrees)
	res.vector = [cos(r), sin(r)]
	res.resetEquations()
	return res
    }
        
    parallelTo (line) {
	// tested
	return this.atAngle(line.angle(0))
    }
    
    along (line) {
	// tested
	return this.through(line.point(0)).parallelTo(line)
    }

    get extension () {	return new Line().along(this) }
    
    perpendicularTo (line) {
	// tested
	return this.atAngle(line.angle(0)+90)
    }
    
    bisectrisse (angle) {
	// tested
	return this.atAngle((angle.start + angle.end)/2)
    }
    
    midPerpendicular (segment) {
	// tested
	return this.through(segment.point(1/2)).perpendicularTo(segment)
    }

    tangentTo(circle, direction = 1) {
	console.assert(this.point(0).distance(circle.center) >= circle.R,
		       "The pivot point of the tangent line is inside the circle!")
	
	const d = segment(this.point(0), circle.center)
	const a = direction * rad2deg(Math.asin(circle.R/d.length))
	return this.atAngle(d.angle(0)+a)
    }

    isParallelTo (line) {
	// tested
	return equalMod(180, line.angle(0),  this.angle(0))
    }

    isPerpendicularTo (line) {
	// tested
	return equalMod(180, line.angle(0) - this.angle(0), 90)
    }

    isTangentTo (circle) {
	return equal(circle.center.distance(this), circle.R)
    }

    isCCW (line) {
	console.assert(line instanceof Line,
		       "isCCW could be computed only for lines")
	return cross(this.vector, line.vector) >= 0
    }        

    
    distance (obj) {
	// tested
	console.assert(obj instanceof Point || obj instanceof Circle,
		      'object must be a point or a circle!')
	
	if (obj instanceof Point)
	    return obj.distance(this)

	return abs(this.distance(obj.center) - obj.R)
    }

    get box () {
	const ts = plane.intersections(this).map(p => this.location(p)).sorted()
	if (ts.length < 2) return Box.mempty
	return Box.mappend(this.point(ts[0]).box, this.point(ts[1]).box)
    }
    
    show (paper) {
	const ts = plane.intersections(this).map(p => this.location(p)).sorted()
	if (ts.length < 2) return
	paper.line([this.point(ts[0]).xy, this.point(ts[1]).xy], this.options)
	
	if (this.isLabeled)
	{
	    const lp = this.point(ts[1]).xy.vsub(this.tangentV(0).scale(2))
	    const lpos = lp.vadd([-0.5, -0.5])
	    const loff = this.normalV(0).scale(this.option('labelOffset'))
	    mkLabel(paper, this.option('label'), lpos.vadd(loff))
	}
	if (this.option('arrow'))
	{
	    const x = this.option('arrowPosition')
	    paper.arrow([this.point(x - 0.1).xy, this.point(x).xy], this.options)
	}
    }

    labelPosition (x) {
	return this.option('labelPosition', x)
    }
    
}


class Ray extends Line {

    constructor(label) {
	super(label)
	const defaults = {
	    'labelOffset' : 1.25
	}
	this.options = Object.assign(defaults, this.options)
    } 

    // ------------------------------------------------------------
    // Figure implementation
    copy () {
	// tested
	const res = new Ray()
	Object.assign(res.options, this.options)
	res.pivot = this.pivot.copy()
	res.vector = this.vector.copy()
	res.resetEquations()
	return res
    }
    
    isEqual (obj) {
	// tested
	return (obj instanceof Ray)
	    && super.isEqual(obj)
    }

    isSimilar (obj) {
	// tested
	return (obj instanceof Ray)
	    && this.start.isEqual(obj.start)
	    && equal(this.tangentV(0), obj.tangentV(0))
    }

    // ------------------------------------------------------------
    // Curve implementation

    isContaining (point) {
	return super.isContaining(point)
	    && this.location(point) >= 0
    }
    
    // ------------------------------------------------------------
    // Ray methods

    get start () { return this.point(0) }

    // ------------------------------------------------------------
    // BoxedFigure implementation 

    get box () {
	const t = this.location(this.intersections(plane)[0])
	return Box.mappend(this.point(0).box, this.point(t).box)
    }
    
    show (paper) {
	const t = this.location(this.intersections(plane)[0])
	paper.line([this.point(0).xy, this.point(t).xy], this.options)
	
	if (this.isLabeled)
	{
	    const lp = this.point(t).xy.vsub(this.tangentV(0).scale(2))
	    const lpos = lp.vadd([-0.5, -0.5])
	    const loff = this.normalV(0).scale(this.option('labelOffset'))
	    mkLabel(paper, this.option('label'), lpos.vadd(loff))
	}
	if (this.option('arrow'))
	{
	    const x = this.option('arrowPosition')
	    paper.arrow([this.point(x - 0.1).xy, this.point(x).xy], this.options)
	}
    }
}



class Segment extends Line {
    constructor(label) {
	super(label)
	const defaults = {
	    'mark' : 0, 
	    'labelPosition' : 1/2, 
	    'labelOffset' : 1.25
	}
	this.type = 'finite'
	this.length = 1
	this.options = Object.assign(defaults, this.options)
    }

    // ------------------------------------------------------------
    // Figure implementation
    copy () {
	// tested
	const res = new Segment()
	Object.assign(res.options, this.options)
	res.pivot = this.pivot.copy()
	res.vector = this.vector.copy()
	res.length = this.length
	res.resetEquations()
	return res
    }

    isEqual (obj) {
	return (obj instanceof Segment) &&
	    super.isEqual(obj)
    }

    isSimilar (segment) {
	//tested
	return (segment instanceof Segment)
	    && equal(this.length, segment.length)
    }

    // ------------------------------------------------------------
    // Curve implementation
    resetEquations () {
	super.resetEquations()
	this.length = this.vector.norm()
	return this
    }
    
    isContaining (point) {
	return super.isContaining(point)
	    && this.location(point) >= 0
	    && this.location(point) <= 1
    }

    // ------------------------------------------------------------
    // Segment methods
   
    get start () { return this.point(0) }
    get end () { return this.point(1) } 
    get middle () { return this.point(1/2) }
    
    extend (t1, t2 = null) {
	//tested
	if (!t2)
	    return this.joining(this.point(0), this.point(t1))
	else
	    return this.joining(this.point(t1), this.point(t2))
    }

    extendToLength (number) {
	// tested
	const p = this.pivot.vadd(this.tangentV(0).scale(number))
	return this.joining(this.start, point(p))
    }

    extendToLine (line) {
	// tested
	var e = line.extension
	if (e.isContaining(this.start))
	    return this.extend(0)
	if (this.isParallelTo(line))
	    return new Ray().through(this.start).parallelTo(this)
	else
	    return this.joining(this.start, e.intersections(this.extension)[0])
    }

    extendTo (obj) {
	console.assert(obj instanceof Curve,
		       "Segment could extended only to a curve!")
	if (obj instanceof Line)
	    return this.extendToLine(obj)
	if (obj instanceof Curve) {
	    let r = new Ray().through(this.start).parallelTo(this),
		int = r.intersections(obj)
	    if (int.length == 0)
		return r
	    else
		return this.joining(this.start, int[0])
	}
	return this
    }
    
    heightTo (line) {
	// tested
	return this.perpendicularTo(line).extendToLine(line)
    }

    tangentTo(circle, direction = 1) {
	// tested
	const d = this.start.distance(circle.center)
	const t = new Ray().through(this.start).tangentTo(circle, direction)
	if (equal(d, circle.R)) {
	    return this.parallelTo(t)
	} else if (d < circle.R) {
	    console.log()
	    return this
	} else {
	    const p = this.parallelTo(t).extendToLength(sqrt(d**2-circle.R**2)).end
	    const a = circle.location(p)
	    return this.joining(this.start, circle.point(a))
	}
    }
    
    internal (points) {
	return points.filter(p => this.isContaining(p))
    }

    flip () { return this.reflectAt(this.middle) }

    get box () {
	const ts = plane.intersections(this).map(p => this.location(p)).sorted()
	if (ts.length < 2) return Box.mempty
	return Box.mappend(this.point(max(0,ts[0])).box, this.point(min(1,ts[1])).box)
    }

    clipBy (curve) {
	let is = [this.start, this.intersections(curve), this.end]
	    .flat()
	var parts = is.zipWith(is.tail(), segment)
	return new Group(parts.filter(p => curve.isEnclosing(p.middle)))
    }
        
    show (paper)
    {
	this.clipBy(plane)
	    .contents
	    .forEach(p => paper.line([p.start.xy, p.end.xy], this.options))
	    
	if (this.isLabeled)
	{
	    const lp = this.point(this.option('labelPosition')).xy
	    const lpos = lp.vadd([-0.5, -0.5])
	    const loff = this.normalV(0).scale(this.option('labelOffset'))
	    mkLabel(paper, this.label(), lpos.vadd(loff))
	}
	if (this.option('arrow'))
	{
	    const x = this.option('arrowPosition')
	    paper.arrow([this.point(x - 0.1).xy, this.point(x).xy], this.options)
	}
	if (this.option('mark') > 0)
	{
	    for (let i = 0; i < this.option('mark'); i++) {
		const m = this.point(1/2).xy.vadd(this.tangentV(0).scale(0.2*i))
		paper.line([m.vadd(this.normalV(0).scale(0.3)), 
			    m.vadd(this.normalV(0).scale(-0.3))], 
			   {'class':'thin'})
	    }
	}
    }

    mark (n) { return this.option('mark', n) }
    arrow () { return this.option('arrow', true) }
    points (t) { return this.option('points', t) }

}

var l


////////////////////////////////////////////////////////////
// Angle

class Angle extends BoxedFigure {
    constructor (value = 'value') {
	super(value)
	const defaults = {
	    'strokes' : 1, 
	    'labelRadius' : 3, 
	    'line-style': style.angle,
	}
	this.options = Object.assign(defaults, this.options)
	this.start = 0
	this.end = Number.isFinite(value) ? value % 360 : 0
	this.vertex = origin
    }
    
    copy () {
	//tested
	const res = new Angle()
	Object.assign(res.options, this.options)
	res.start = this.start
	res.end = this.end
	res.vertex = this.vertex.copy()
	return res
    }

    isEqual (obj) {
	//tested
	return (obj instanceof Angle)
	    && equalMod(360, this.start, obj.start)
	    && equalMod(360, this.end, obj.end)
	    && this.vertex.isEqual(obj.vertex)
    }

    isSimilar (obj) {
	//tested
	return (obj instanceof Angle)
	    && equalMod(360, this.value, obj.value)
    }

    on (line) {
	//tested
	const res = this.copy()
	res.vertex = point(line.pivot)
	res.start = line.angle(0)
	res.end = res.start + this.value
	return res
    }

    at (point) {
	//tested
	const res = this.copy()
	res.vertex = point
	return res
    }
    
    transform (T) {
	//tested
	const res = this.copy()
	res.vertex = this.vertex.transform(T)
	res.start = this.startRay.transform(T).angle(0)
	res.end = this.endRay.transform(T).angle(0)
	return res
    }
    
    get value () { return (2*360 + this.end - this.start) % 360 }
    get startRay () {
	return new Ray().through(this.vertex).atAngle(this.start)
    }
    get endRay () {
	return new Ray().through(this.vertex).atAngle(this.end)
    }
    get isRight () {
	return equalMod(360, this.value, 90) || cos(deg2rad(this.value)) < 1e-14
    }

    setValue (angle) {
	//tested
	const res = this.copy()
	res.end = this.end + angle
	return res	
    }
    
    vertical () {
	// tested 
	return this.rotateAt(this.vertex, 180)
    }

    adjacent () {
	// tested 
	const res = this.copy()
	res.start = this.end
	res.end = (this.start + 180) % 360
	return res
    }

    complement () {
	// tested 
	const res = this.copy()
	res.start = this.end
	res.end = this.start
	return res
    }
    
    within (point1, point2, point3) {
	// tested
	const res = this.copy()
	res.start = ray(point2, point1).angle(0)
	res.end = ray(point2, point3).angle(0)
	res.vertex = point2
	return res
    }
    
    between (line1, line2) {
	// tested
	const res = this.copy()
	res.start = line1.angle(0)
	res.end = line2.angle(0)
	res.vertex = line1.point(0)
	return res
    }
   
    show (p) {
	if (this.isRight)
	{
	    const p0 = this.vertex.xy
	    const p1 = this.startRay.tangentV(0)
	    const p2 = this.endRay.tangentV(0)
	    p.line([p1, p1.vadd(p2), p2].map(p => p0.vadd(p)), this.options)
	    return
	}

	for(let i = 0; i < this.options['strokes']; i++)
	    p.arc(this.vertex.xy, 2-i*0.3, this.start, this.end, this.options)

	if (this.isLabeled)
	{
	    let l = this.option('label')
	    if (l == 'value')
		l = abs(round(this.value))+'°'
	    const r = this.option('labelRadius')
	    const lpos = new Ray().bisectrisse(this).tangentV(0).scale(r)
		  .vadd(this.vertex.xy).vadd([-0.5,-0.7])
	    mkLabel(p,l,lpos)
	    
	}
    }

    strokes (n) { return this.option('strokes', n) }
}

////////////////////////////////////////////////////////////
// Polygon

class Polygon extends Curve {
    constructor(label) {
	super(label)
	const defaults = {
	    'labelPosition' : 'automatic',
	    'points' : false,
	    'segmentLabels':[],
	    'point-style': style.point,
	    'line-style': style.polygon
	}
	this.options = Object.assign(defaults, this.options)
	this.type = 'finite'
	this.vertices = []
	this.verticesLocations = []
    }

    //------------------------------------------------------------
    // Figure implementation
    
    copy () {
	const res = new Polygon()
	Object.assign(res.options, this.options)
	res.type = this.type
	res.vertices = this.vertices.map(v => v.copy())
	res.resetEquations()
	return res
    }

    transform (T) {
	const res = this.copy()
	res.vertices = this.vertices.map(v => v.transform(T))
	res.resetEquations()
	return res
    }

    isEqual (obj) {
	return (obj instanceof Polygon)
	    && equal(this.vertices, obj.vertices)
    }

    //------------------------------------------------------------
    // Curve implementation

    resetEquations () {
	if(this.vertices.length < 2) return this
	let pts = this.vertices.map(p => p.xy)
	const par = naturalParametrization(pts, this.type)
	this.point = s => point(par.eqn(s))
	this.length = par.length
	this.verticesLocations = par.pointLocations
	this.equation = undefined
	return this
    }

    isContaining (point) {
	return this.sides.some(s => s.isContaining(point)) 
    }

    location (point) {
	return findMinimum(x => point.distance(this.point(x)),0,1).minimum
    }
    
    flip () {
	let res = this.copy()
	res.vertices = this.vertices.reversed()
	this.resetEquations()
	return this
    }

    intersections (obj) {
	return this.sides.mapappend(s => s.intersections(obj)
				    .filter(p => s.isContaining(p) && obj.isContaining(p)))
    }
    
    //------------------------------------------------------------
    // Polygon methods
    
    setType (t) {
	this.type = t
	return this
    }

    get number () { return this.vertices.length }
    
    get sides () {
	let res = [], vs
	if (this.isClosed)
	    vs = this.vertices.concat([this.vertices[0]])
	else
	    vs = this.vertices
	for(let i = 0; i < vs.length-1; i++) {
	    const s = this.options.segmentLabels[i]
	    res.push(new Segment(s).joining(vs[i], vs[i+1]))
	}
	return res
    }
    
    vertex (i) {
	if (this.isClosed)
	    return this.vertices[(this.number + i - 1) % this.number]
	else {
	    console.assert(i > 0 && i <= this.number,
			   `The polygon has ${this.number} vertices, requested ${i}.`)
	    return this.vertices[i - 1]
	}
    }

    vertexLocation (i) {
	if (this.isClosed)
	    return this.verticesLocations[(this.number + i - 1) % this.number]
	else {
	    console.assert(i > 0 && i <= this.number,
			   `The polygon has ${this.number} vertices, requested ${i}.`)
	    return this.verticesLocations[i - 1]
	}
    }
    
    side (i) {
	if (this.isClosed)
	    return this.sides[(this.number + i - 1) % this.number]
    	else {
	    console.assert(i > 0 && i <= this.number-1,
			   `The polygon has ${this.number-1} sides, requested ${i}.`)
	    return this.sides[i - 1]
	}
    }
    
    vertexAngle (i) {
	if (this.isClosed) 
	    console.assert(i > 0 && i <= this.number-2,
			   `The polygon has ${this.number-2} angles, requested ${i}.`)
	let v1 = this.vertex(i-1),
	    v2 = this.vertex(i),
	    v3 = this.vertex(i+1)
	return new Angle().within(v1,v2,v3)
    }

    labelVertices (labels) {
	this.vertices.map((v,i) => v.label(labels[i]))
	return this
    }
    
    labelSegments (labels) { return this.option('segmentLabels',labels) }

    at (point, vertex_n = 1) {
	return this.superpose(this.vertex(vertex_n), point)
    }
    
    through (points) {
	const res = this.copy()
	res.vertices = points.copy()
	res.resetEquations()
	return res
    }

    on (segment, side_n = 1) {
	return this.scaleAt(this.vertex(side_n), segment.length/this.side(side_n).length)
	    .align(this.side(side_n), segment)
	    .at(segment.start, side_n)
    }

    static parametric (fn, domain) {
	return new Polygon().through(domain.map(s => point(fn(s))))
    }

    static polar (fn, n = 50, start = 0, end = 2*pi) {
	let step = (end - start)/n
	return this.parametric(t => [cos(t), sin(t)].scale(fn(t)),
			       range(start, end+step, step))
    }

    static regular (n, r = 1) {
	var fn = t => [r*sin(2*pi*t/n), r*cos(2*pi*t/n)]
	var res = Polygon.parametric(fn, range(0, n)).closed()
	return res
    }

    get box () {
	return this.vertices.foldMap(v => v.box, Box)
    }
    
    show (paper)
    {
	this.sides.forEach(s => s.show(paper))

	return 
	let pts = this.vertices.map(v => v.xy)
	if (this.isClosed)
	    pts = pts.concat([pts[0]])
	
	paper.listLinePlot(pts, this.options)

	if (this.option('points'))
	    this.vertices.forEach(s => s.show(paper))

    }

    fill (c) {
	return this.lineStyle({'fill': c})
    }

    opacity (n) {
	return this.lineStyle({'fill-opacity': n})
    }
    
    points (bool) {
	this.options['points'] = bool
	return this
    }

    noline () {
	return this.lineStyle({'stroke': 'none'})
    }
    
    joined (bool) {
	this.options['joined'] = bool
	return this
    }  
}

class Triangle extends Polygon {
    constructor (point1 = null, point2 = null, point3 = null) {
	super()
	const defaults = { }
	this.options = Object.assign(defaults, this.options)
	let s
	if (!point1) {
	    s = Polygon.regular(3)
	} else if (!point2) {
	    s = Polygon.regular(3).at(point1)
	} else if (!point3) {
	    s =Polygon.regular(3).on(segment(point1,point2))
	} else {
	    s = new Polygon().through([point1, point2, point3])
	}
	this.vertices = s.vertices
	this.type = 'closed'
	return this.resetEquations()
    }

    copy () { return super.copy(new Triangle()) }

    withAngles (angle1, angle2 = angle1) {
	console.assert(!equalMod(180, angle1 + angle2, 0), 
		       `Angles ${angle1} and ${angle2} do not form a triangle`)
	const a = this.vertex(1),
	      b = this.vertex(2)
	return new Triangle(a, b, new Point().azimuth(a, angle1, b, angle2))
    }

    withHeight (height, vertex_n = 2) {
	const h = this.height(vertex_n)
	return this.stretchAt(this.vertex(vertex_n - 1), h, height/h.length)
    }
    
    median (i) {
	const v = this.vertex(i),
	      s = this.side(i+1)
	return new Segment().joining(v, s.point(1/2))
    }
    
    height (i) {
	const v = this.vertex(i),
	      s = this.side(i+1)
	return new Segment().through(v).heightTo(s)
    }

    bisectrisse (i) {
	const v = this.vertex(i),
	      a = this.vertexAngle(i),
	      s = this.side(i+1)
	return new Segment().through(v).bisectrisse(a).extendToLine(s)
    }

}

class Quadrilateral extends Polygon {
    constructor () {
	super()
	const defaults = {
	    'closed' : true
	}
	this.options = Object.assign(defaults, this.options)
	this.type = 'closed'
    }

    copy () { return super.copy(new Quadrilateral()) }

    through (points) {
	let res = this.copy()
	res.vertices = points.take(4)
	res.resetEquations()
	return res
    }
    
    transform (T) {
	const res = this.copy(),
	      s = super.transform(T)
	res.vertices = s.vertices
	return res
    }

    diagonal (i) { return segment(this.vertex(i),this.vertex(i+2)) }
}


class Square extends Quadrilateral {

    constructor (side = 1) {
	super()
	const defaults = { }
	this.options = Object.assign(defaults, this.options)
	const a = side/2
	this.vertices = [point([-a,-a]), point([a,-a]),
			 point([a,a]), point([-a,a])]
	this.resetEquations()
    }

    copy () { return super.copy(new Square()) }

    through (p1,p2) {
	return new Square(segment(p1,p2).length)
    }
    
    sideLength (number) {
	const res = this.copy(),
	      a = this.side(1).length
	return res.scaleAt(this.vertex(1), number/a)
    }    
}

class Rectangle extends Quadrilateral {

    constructor (side1 = 1, side2 = side1) {
	super()
	const defaults = { }
	this.options = Object.assign(defaults, this.options)
	const a = side1/2, b = side2/2
	this.vertices = [point([-a, -b]), point([a, -b]),
			 point([a, b]), point([-a, b])]
	this.resetEquations()
    }

    copy () { return super.copy(new Rectangle()) }

    through (p1, p2, p3) {
	return new Rectangle(segment(p1, p2).length, segment(p2, p3).length)
    }

    get width () { return this.side(1).length }
    get height () { return this.side(2).length }
    
    sideLength (width, height) {
	const res = this.copy()
	return res.scaleAt(this.vertex(1), width/this.width, height/this.height)
    }
}

class Sector extends Polygon {
    constructor (center = origin
		 , radius = 1
		 , startAngle = 0
		 , endAngle = 360) {
	super()
	const defaults = { }
	this.options = Object.assign(defaults, this.options)
	const c = new Circle(radius).at(center)
	this.vertices = range(startAngle, endAngle + 2, 2)
	    .map(a => c.point(deg2rad(a)/2/pi)).concat([center])
	this.type = 'closed'
	this.resetEquations()
    }

    copy () {
	const res = new Sector()
	res.vertices = this.vertices.map(v => v.copy())
	Object.assign(res.options, this.options)
	return res
    }
    
}

class CircleSegment extends Polygon {
    constructor (center = origin
		 , radius = 1
		 , startAngle = 0
		 , endAngle = 360) {
	super()
	const defaults = { }
	this.options = Object.assign(defaults, this.options)
	const c = new Circle(radius).at(center)
	this.vertices = range(startAngle, endAngle + 2, 2)
	    .map(a => c.point(deg2rad(a)))
	this.type = 'finite'
	this.resetEquations()
    }

    copy () {
	const res = new CircleSegment()
	res.vertices = this.vertices.map(v => v.copy())
	Object.assign(res.options, this.options)
	return res
    }    
}

////////////////////////////////////////////////////////////
// Circle

class Circle extends Curve {
    constructor(r = 1) {
	super()
	const defaults = {
	    'style': style.circle
	}
	this.options = Object.assign(defaults,this.options)
	this.type = 'closed'
	this.center = origin.copy()
	this.R = r
	this.phase = 0
	this.orientation = Math.sign(cross([1, 0], [0,1]))
	this.resetEquations()
    }

    //------------------------------------------------------------
    // Figure implementation
    copy () {
	const res = new Circle(this.R)
	Object.assign(res.options, this.options)
	res.R = this.R
	res.center = this.center.copy()
	res.phase = this.phase
	res.orientation = this.orientation
	res.resetEquations()
	return res
    }
    
    transform (T) {
	const r = this.radius(0).transform(T)
	const t = this.tangentV(0)
	const res = new Circle()
	res.R = r.length
	res.center = r.start
	res.phase = res.location(r.end)
	res.orientation = Math.sign(cross(r.vector, trans(T,t)))
	res.resetEquations()
	return res
    }

    isEqual (obj) {
	return (obj instanceof Circle) &&
	    equal(this.center, obj.center) &&
	    equal(this.R, obj.R)
    }

    isSimilar (obj) {
	return this.isEqual(obj)
    }

    //------------------------------------------------------------
    // Curve implementation
    resetEquations () {
	const c = this.center,
	      r = this.R,
	      ph = this.phase,
	      w = this.orientation
	this.point = t => point([c.x+this.R*cos(2*pi*(w*t + ph)),
				 c.y+this.R*sin(2*pi*(w*t + ph))])
	this.length = 2*this.R*pi
	this.equation = xy => equal(point(xy).distance(this.center), this.R)
    }

    tangentV (t) {
	const ph = this.phase,
	      w = this.orientation
	return [-w*sin(2*pi*(w*t + ph)), w*cos(2*pi*(w*t + ph))]
    }

    location (point) {
	return deg2rad(line(this.center, point).angle(0))/(2*pi)
    }
    
    isEnclosing (point) {
	return point.distance(this.center) <= this.R
    }

    flip () {
	let res = this.copy()
	Object.assign(res.options, this.options)
	res.orientation = -this.orientation
	return res
    }
    
    //------------------------------------------------------------
    // Circle methods

    radius (s = 0) { return segment(this.center, this.point(s)) }

    at (point) {
	const res = this.copy()
	res.center = point
	res.resetEquations()
	return res
    }

    intersectionC (that) {
	const r0 = this.R,
	      r1 = that.R,
	      r = segment(this.center, that.center),
	      R = r.length
	if (R == 0)
	    return []
	if (R > r0 + r1 || R < abs(r0-r1))
	    return []
	const a = (r0**2 - r1**2)/(2*R),
	      b = 1/2*sqrt(2*(r0**2 + r1**2) - 4*a**2 - R**2),
	      m = r.middle.xy.vadd(r.tangentV(0).scale(a))
	if (b == 0)
	    return [point(m.vadd(r.normalV(0).scale(b)))]
	else 
	    return [point(m.vadd(r.normalV(0).scale(b))), point(m.vadd(r.normalV(0).scale(-b)))]
    }

    intersectionL (line) {
	const h = new Segment().through(this.center).heightTo(line)
	if (equal(h.length, this.R))
	    return [h.end]
	if (h.length > this.R)
	    return []
	const x = sqrt(this.R**2 - h.length**2),
	      s = new Segment().through(h.end).parallelTo(line).extend(-x,x)
	return [s.end,s.start]
    }

    intersections (obj) {
	var res = []
	if (obj instanceof Line)
	    res = this.intersectionL(obj)
	if (obj instanceof Circle)
	    res = this.intersectionC(obj)
	if (obj instanceof Polygon)
	    res = obj.intersections(this)
	return res.filter(p => obj.isContaining(p) && this.isContaining(p))
	    .sorted((p1,p2) => this.location(p1) - this.location(p2))
    }
    
    through1 (point) {
	this.R = segment(this.center, point).length
	this.resetEquations()
	return this.copy()
    }

    through2 (p1, p2) {
	const d = p1.distance(p2)
	if (d > this.R*2) {
	    this.R = d/2
	    this.center = segment(p1,p2).middle
	} else {
	    const c1 = new Circle(this.R).at(p1),
		  c2 = new Circle(this.R).at(p2)
	    this.center = c1.intersectionC(c2)[0]
	}
	this.resetEquations()
	return this.copy()
    }

    through3 (p1,p2,p3) {
	const l1 = new Line().midPerpendicular(segment(p1,p2)),
	      l2 = new Line().midPerpendicular(segment(p2,p3))

	console.assert(!l1.isParallelTo(l2),
		       "Can't build a circle, three points are aligned.")
	
	this.center = new Point().intersections(l1,l2)[0]
	this.R = this.center.distance(p1)
	this.resetEquations()
	return this.copy()
    }

    through (point1, point2, point3) {
	if (!point2  && !point3)
	    return this.through1(point1)
	else if (!point3)
	    return this.through2(point1, point2) 
	else
	    return this.through3(point1, point2, point3)
    }

    distance (obj) {
	console.assert( obj instanceof Point ||
			obj instanceof Line ||
			obj instanceof Circle,
			'object must be a line, point or a circle!')
	
	if (obj instanceof Point || obj instanceof Line)
	    return obj.distance(this)

	return abs(this.distance(obj.center) - obj.R - this.R)
    }


    get box () {
	return range(0,2*pi,pi/2).foldMap(t => this.point(t).box, Box)
    }
    
    show (paper) {
	paper.disk(this.center.xy, this.R, this.options)
    }
}

////////////////////////////////////////////////////////////
// Text
class Text extends BoxedFigure {
    constructor (text)
    {
	super(text)
	const defaults = {
	    'labelOffset': [0, 0],
	    'label-style': style.text
	}
	this.options = Object.assign(defaults, this.options)
	this.pivot = origin
	this.angle = 0
    }

    copy () {
	const res = new Text()
	Object.assign(res.options, this.options)
	res.pivot = this.pivot.copy()
	res.angle = this.angle
	return res
    }
    
    at (point) {
	const res = this.copy()
	res.pivot = point
	return res
    }

    on (line, t = 0) {
	let res = this.copy()
	res.pivot = line.point(t)
	return res.atAngle(line.angle(0)).option('labelOffset',line.normalV(0).scale(2/3))
    }

    transform (T) {
	const res = this.copy()
	res.pivot = this.pivot.transform(T)
	return res
    }

    atAngle (a) {
	const res = this.copy()
	res.angle = a
	return res
    }

    get length () {
	return this.label().length
    }
    
    get box () {
	return new Rectangle(this.length*0.75, 1)
	    .at(this.pivot)
	    .rotateAt(this.pivot, this.angle)
	    .box
    }

    show (paper) {
	if (this.isLabeled) {
	    const l = this.options.label,
		  lp = this.pivot.xy.vadd(this.option('labelOffset'))
	    mkLabel(paper, l, lp
		    , Object.assign(this.options, {'style' : style.text
						   ,'angle' : this.angle + 1e-10}))
	}
    }
    
}

////////////////////////////////////////////////////////////
// Scale

class Scale extends BoxedFigure {
    constructor(steps = 2, labelF = x => x, format = fmt.fixed2) {
	super()
	const defaults = {
	    'tickLength' : 0.5,
	    'labelF' : labelF,
	    'format' : format,
	    'labels' : true,
	    'range' : range(0, 1+1/steps, 1/steps),
	    'unit' : 1,
	    'closed' : false
	}
	this.options = Object.assign(defaults, this.options)
	this.steps = steps
	this.curve = new Segment()
    }

    copy () {
	const res = new Scale()
	Object.assign(res.options, this.options)
	res.steps = this.steps
	res.curve = this.curve.copy()
	return res	
    }

    transform (T) {
	const res = this.copy()
	res.curve = this.curve.transform(T)
	return res	
    }
    
    on (curve) {
	console.assert(curve instanceof Curve,
		       "Scale must lay on a curve.")
	var res = this.copy()
	Object.assign(res.options, this.options)
	res.curve = curve.copy()
	return res
    }

    lableFunction (func) { return this.option('labelF', func) }
    unit (u) { return this.option('unit', u || 1) }
    closed (bool = true) { return this.option('closed', bool) }
    format (func) { return this.option('format', func) }
    tickLength (v) { return this.option('tickLength', v) }
    noLabels () { return this.option('labels', false) }

    circular() { return this.unit(this.steps).closed() }
    
    range ([a, b]) {
	let s = (b-a)/this.steps
	return this.option('range', range(a, b + s, s))
    }

    flip () {
	let res = this.copy()
	Object.assign(res.options, this.options)
	res.curve = this.curve.flip()
	return res
    }
    
    get group () {
	const tick = x => {
	    let p = this.curve.point(x)
	    let s = new Segment().through(p)
		.atAngle(this.curve.angle(x) + 90)
		.extendToLength(this.tickLength())
	    let l = false, u = this.options.unit
	    if (this.options.labels)
		l = s.end.invisible().label(this.format()(this.lableFunction()(u*x)))
		.labelOffset(s.tangentV(0).scale(abs(this.tickLength())))
	    return l ? new Group([s,l]) : s
	}
	if (this.options.closed)
	    return new Group(this.options.range.most().map(tick))
	else
	    return new Group(this.options.range.map(tick))
    }

    show (paper) { this.group.show(paper) }
}

////////////////////////////////////////////////////////////
// predefined objects

class Chart {
    constructor (fig)
    {
	this.svg = new SVG(fig, Object.assign({'size':600},style.svg))
	this.paper = new Graphics(this.svg,
				  {'size':600,'aspectRatio':1,
				   'left-margin':10,'right-margin':10,
				   'top-margin':10,'botom-margin':10,
				   'class':'chart'})
	    .xRange(xRange)
  	    .yRange(yRange)
    }

    save (name) { this.svg.save(name) }
    
    clear() { return this.paper.cleanPaper() }

    adjustSize () {
	this.paper.adjustSize()
	return this
    }
    
    put ()
    {
	[...arguments].flat().forEach(o => o.show(this.paper))
	return this
    }
}

////////////////////////////////////////////////////////////
// smart constructors

const point = ([x, y]) => new Point().at([x, y])

function anyPoint(n) {
    const x = () => round(Math.random()*paperSize)-paperSize/2
    const y = () => round(Math.random()*paperSize)-paperSize/2
    const res = range(n).map(() => new Point().at([x(),y()]))
    return new Group(res)
}

function line (point, point_or_angle = 0) {
    if (point_or_angle instanceof Point)
	return new Line().joining(point, point_or_angle)
    else
	return new Line().through(point).atAngle(point_or_angle)
}

function ray (point, point_or_angle = 0) {
    if (point_or_angle instanceof Point)
	return new Ray().joining(point, point_or_angle)
    else
	return new Ray().through(point).atAngle(point_or_angle)
}

const segment = (a, b) => new Segment().joining(a, b)

const angle = (a,b,c) => new Angle().within(a,b,c)

function mkLabel(p, l, pnt, opts)
{
    let options = Object.assign({'at':pnt,'style':style.label}, opts)
    return p.label(l, options)
}

////////////////////////////////////////////////////////////
// predefined objects

const origin = new Point('O').at([0,0]).option('labelOffset',[-1,-1])
const Ox = new Line().through(origin).atAngle(0)
const Oy = new Line().through(origin).atAngle(90)
//gconst plane = new Square(paperSize)
const plane = new Circle(paperSize/2)
