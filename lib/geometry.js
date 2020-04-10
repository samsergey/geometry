////////////////////////////////////////////////////////////
// math

function angleV([x,y])
{
    if (x == 0 && y == 0) return 0
    if (x == 1 && y == 0) return 0
    if (x == 0 && y == 1) return 90
    if (x == -1 && y == 0) return 180
    if (x == 0 && y == -1) return 270
    var a = rad2deg(Math.atan(y/x))
    if (x >= 0 && y >= 0)
	return a
    if (x <= 0 && y >= 0)
	return 180 + a
    if (x <= 0 && y < 0)
	return 180 + a
    if (x > 0 && y <= 0)
	return 360 + a
}

function lineEquation([x1,y1],[x2,y2],s)
{
    var v = [x2-x1,y2-y1], s = s || 1
    return t => [x1,y1].vadd(v.scale(t*s))
}

function intersectionV([x1,y1],[v1x,v1y],[x2,y2],[v2x,v2y])
{
    var D = v1y*v2x-v1x*v2y
    var D1 = v1x*y1-v1y*x1
    var D2 = v2y*x2-v2x*y2
    if (D == 0) return [1/0,1/0]
    return [-(v1x*D2+v2x*D1)/D, -(v1y*D2+v2y*D1)/D]
}

////////////////////////////////////////////////////////////
// Figure and transformations

class Figure {
    constructor () {
	this.options = {}
    }
   
    augment ([x,y]) { return [x,y,1] }
    pure ([x,y,_]) { return [x,y] }
    trans (T,v) { return this.pure(vmul(T,this.augment(v))) }
    rotT (a) { return [[cos(a),-sin(a),0],[sin(a),cos(a),0],[0,0,1]] }
    reflectT (a) { return [[cos(2*a),sin(2*a),0],[sin(2*a),-cos(2*a),0],[0,0,1]] }
    shiftT ([dx,dy]) { return [[1,0,dx],[0,1,dy],[0,0,1]] }
    scaleT (a,b) { return [[a,0,0],[0,b,0],[0,0,1]] }
    flipHT (a) { return [[-1,0,0],[0,1,0],[0,0,1]] }
    flipVT (a) { return [[1,0,0],[0,-1,0],[0,0,1]] }
    flipCT (a) { return [[-1,0,0],[0,-1,0],[0,0,1]] }
    
    transform (T) {
	console.error("transform is unimplemented")
    }
 
    shift(d) {
	return this.transform(this.shiftT(d))
    }
  
    rotate(a) {
	return this.transform(this.rotT(deg2rad(a)))
    }

    reflect (a) {
	return this.transform(this.reflectT(deg2rad(a)))
    }
    
    rotateAt(p,a) {
	var pt = p.pt
	return this.shift(pt.flip()).rotate(a).shift(pt)
    }

    reflectAt(v) {
	if (Point.isPoint(v)) {
	    return this.rotateAt(v,180)
	}
	var a = v.angle
	var pt = v.pivot
	return this.shift(pt.flip()).reflect(a).shift(pt)
    }

    option (opt, x) {
	if (x) {
	    this.options[opt] = x
	    return this
	}
	else
	   return this.options[opt]
    }

    style (c) { return this.option('class', c) }

    get isLabeled () { return !!this.options['label'] }
}

////////////////////////////////////////////////////////////
// Point

class Point extends Figure {
    constructor (label,opts)
    {
	super()
	var defaults = {
	    'pointSize' : 3,
	    'label' : label,
	    'labelOffset': [0,1],
	    'class' : 'regular'
	}
	this.valueOf = 'point'	
	this.options = Object.assign(defaults,opts || {})
	this.pt = [0,0]
    }

    static isPoint(obj) {
	return obj.valueOf == 'point'
    }

    copy(p) {
	this.pt = p.pt
	if (p.option('label').length > 0)
	    this.option('label', p.option('label') + "'")
	return this
    }
    
    transform (T) {
	this.pt = this.trans(T, this.pt)
	return this
    }

    get x () { return this.pt[0] }
    get y () { return this.pt[1] }
    get isInside () {
	return this.x > xRange[0] && this.x < xRange[1] &&
  	       this.y > yRange[0] && this.y < yRange[1]
    }
    
    at ([x,y]) {
	this.pt = [x,y]
	return this
    }
    
    onLine (line,t) {
	this.at(line.eqn(t))
	return this
    }

    start (s) { return this.onLine(s,0) }
    middle (s) { return this.onLine(s,1/2) }
    end (s) { return this.onLine(s,1) }
    
    between (p1,p2,t) {
	return this.onLine(new Line().joining(p1,p2),t)
    }

    intersection (s1,s2) {
	if (!(Line.isLine(s1) && Line.isLine(s2)))
	    console.error('intersection of non vectors')
	return this.at(s1.intersection(s2).pt)
    }

    azimuth (p1,a1,p2,a2) {
	var s = new Line().joining(p1,p2)
	var s1 = new Line().through(p1).atAngle(s.angle + a1)
	var s2 = new Line().through(p2).atAngle(s.angle + 180 - a2)
	return this.intersection (s1,s2)
    }

    label (l) {
	this.option('label', l)
	return this
    }

    labelOffset (o) {
	this.option('labelOffset', o.normalize().scale(1.2))
	return this
    }
    
    show (p) {
	if (this.isInside)
	    p.listPlot([this.pt],this.options)
	
	if (this.isLabeled) {
	    var l = this.option('label')
	    var lp = this.pt.vadd([-0.3*l.length, -0.5])
	    p.label(l, {'at' : lp.vadd(this.option('labelOffset'))})
	}
    }
}

const point = ([x,y]) => new Point().at([x,y])

////////////////////////////////////////////////////////////
// Line

class Line extends Figure {
    constructor(label,opts) {
	super()
	var defaults = {
	    'arrow' : false,
	    'arrowPosition' : 1,
	    'label' : label,
	    'labelPosition' : 5,
	    'labelOffset' : 1.25,
	    'class' : 'regular'
	}
	this.valueOf = 'line'
	this.options = Object.assign(defaults,opts || {})
	this.pivot = [0,0]
	this.vector = [1,0]
	this.eqn = t => [t,0]
	this.resetEqn()
    }

    static isLine (obj) {
	return obj.valueOf == 'line'
    }

    copy(v) {
	this.pivot = v.pivot
	this.vector = v.vector
	this.resetEqn()
	return this
    }
    
    transform (T) {
	this.pivot = this.trans(T, this.pivot)
	var e = this.trans(T, this.end)
	this.vector = this.e.vsub(this.pivot)
	return this
    }
     
    get unit () { return this.vector.normalize() }
    get norm () { return [-this.unit[1],this.unit[0]] } 
    get angle () { return angleV(this.unit) }

    resetEqn() {
	this.eqn = lineEquation(this.pivot, this.pivot.vadd(this.vector))
    }

    joining(p1, p2) {
	this.pivot = p1.pt
	this.vector = p2.pt.vsub(this.pivot)
	this.resetEqn()
	return this
    }

    scale (s) {
	this.vector = this.vector.scale(s)
	this.resetEqn()
    }
    
    at(p) {
	this.pivot = p
	this.resetEqn()
	return this
    }

    location (p) {
	return p.vsub(this.pivot).dot(this.unit)/this.vector.norm()
    }
    
    atAngle(a) {
	var r = deg2rad(a)
	this.vector = [cos(r), sin(r)]
	this.resetEqn()
	return this
    }

    isParallelTo (l) {
	return abs (l.angle - this.angle) % 180 == 0
    }
	
    intersection (l) {
	var pt = intersectionV(this.pivot, this.unit, l.pivot, l.unit)
	return new Point().at(pt)	
    }
    
    through (p) {
	return this.at(p.pt)
    }

    parallelTo (l) {
	return this.atAngle(l.angle)
    }

    perpendicularTo (l) {
	return this.atAngle(l.angle+90)
    }

    bisectrisse (an) {
	return this.atAngle((an.start + an.end)/2)
    }
   
    show(p) {
	var ts = borders.map(b => this.location(this.intersection(b).pt))
	var t2 = ts.filter(x => x > 0).min()
	var t1 = ts.filter(x => x < 0).max()

	p.listLinePlot([this.eqn(t1), this.eqn(t2)], this.options)
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).vsub(this.unit.scale(2))
	    var lpos = lp.vadd([-0.5, -0.5])
	    var loff = this.norm.scale(this.option('labelOffset'))
	    p.label(this.option('label'), {'at' : lpos.vadd(loff)})
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition')
	    p.arrow([this.eqn(x - 0.1), this.eqn(x)], this.options)
	}
    }
}

const line = (a,b) => new Line().joining(a,b)

class Ray extends Line {

    constructor(label,opts) {
	super(label,opts)
	var defaults = {
	    'class' : 'regular'
	}
	this.options = Object.assign(defaults,opts || {})
    } 
  
    flip () {
	return this.atAngle(this.angle+180)
    }
  
    show(p) {
	var ts = borders.map(b => this.location(this.intersection(b).pt))
	var t2 = ts.filter(x => x > 0).min()

	p.listLinePlot([this.eqn(0), this.eqn(t2)], this.options)
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).vsub(this.unit.scale(2))
	    var lpos = lp.vadd([-0.5, -0.5])
	    var loff = this.norm.scale(this.option('labelOffset'))
	    p.label(this.option('label'), {'at' : lpos.vadd(loff)})
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition')
	    p.arrow([this.eqn(x - 0.1), this.eqn(x)], this.options)
	}
    }
}

const ray = (a,b) => new Ray().joining(a,b)

class Segment extends Line {
    constructor(label,opts) {
	super(label,opts)
	var defaults = {
	    'mark' : 0,
	    'labelPosition' : 1/2,
	    'labelOffset' : 1.25,
	    'class' : 'regular'
	}
	this.options = Object.assign(defaults,opts || {})
    }

    get start () { return point(this.eqn(0)) }
    get end () { return point(this.eqn(1)) } 
    get length () { return this.vector.norm() } 
    
    extend(t1,t2)
    {
	this.pivot = this.eqn(t1)
	this.vector = this.eqn(t2).vsub(this.pivot)
	this.resetEqn()
	return this
    }
    
    extendToLine(v)
    {
	return this.joining(this.start, this.intersection(v))
    }

    extendToLength(a)
    {
	var p = this.pivot.vadd(this.unit.scale(a))
	return this.joining(this.start, point(p))
    }

    heightTo(l)
    {
	return this.perpendicularTo(l).extendToLine(l)
    }
    
    show(p)
    {
	p.listLinePlot([this.eqn(0), this.eqn(1)], this.options)
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(option('lablePosition'))
	    var lpos = lp.vadd([-0.5, -0.5])
	    var loff = this.norm.scale(this.option('labelOffset'))
	    p.label(this.option('label'), {'at' : lpos.vadd(loff)})
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition')
	    p.arrow([this.eqn(x - 0.1),this.eqn(x)],this.options)
	}
	if (this.option('mark') > 0)
	{
	    for (var i = 0; i < this.option('mark'); i++) {
		var m = this.eqn(1/2).vadd(this.unit.scale(0.2*i))
		p.line([m.vadd(this.norm.scale(0.3)),
			m.vadd(this.norm.scale(-0.3))],
		       {'class':'thin'})
	    }
	}
    }

    mark (n) { return this.option('mark', n) }

}

const seg = (a,b) => new Segment().joining(a,b)


////////////////////////////////////////////////////////////
// Angle

class Angle extends Figure {
    constructor (label,opts) {
	super()
	var defaults = {
	    'strokes' : 1,
	    'labelRadius' : 3,
	    'label' : label
	}
	this.options = Object.assign(defaults,opts || {})
	this.start = 0
	this.end = 0
	this.vertex = origin
    }

    copy(an) {
	this.start = an.start
	this.end = an.end
	this.vertex = an.vertex
	return this
    }

    transform (T) {
	this.vertex = this.vertex.transform(T)
	this.start = this.startRay.transform(T).angle()
	this.end = this.endRay.transform(T).angle()
	return this
    }
    
    get value () { return (this.end - this.start) % 360 }
    get startRay () {
	return new Ray().through(this.vertex).atAngle(this.start)
    }
    get endRay () {
	return new Ray().through(this.vertex).atAngle(this.end)
    }
    get isRight () {
	return this.value == 90 || cos(deg2rad(this.value)) < 1e-14
    }
    
    atPoint (p) {
	this.vertex = p
	return this
    }
  
    vertical () {
	this.start = (this.start + 180) % 360
	this.end = (this.end + 180) % 360
    }

    adjacent () {
	[this.start,this.end] = [this.end,(this.start + 180) % 360]
	return this
    }

    complement () {
	[this.start,this.end] = [this.end, this.start]
	return this
    }

    equalTo (a) {
	this.start = a.start
	this.end = a.end
	return this
    }

    verticalTo (a) {
	return this.equalTo(a).vertical()
    }

    adjacentTo (a) {
	return this.equalTo(a).adjacent()
    }

    complementTo (a) {
	return this.equalTo(a).complement()
    }
    
    within (p1,o,p2) {
	this.start = ray(o,p1).angle
	this.end = ray(o,p2).angle
	this.vertex = o
	return this
    }
    
    between (s1,s2) {
	this.start = s1.angle
	this.end = s2.angle
	this.vertex = point(s1.eqn(0))
	return this
    }

    show (p) {
	if (this.isRight)
	{
	    var p0 = this.vertex.pt
	    var p1 = this.startRay.unit
	    var p2 = this.endRay.unit
	    p.listLinePlot([p1,p1.vadd(p2),p2].map(p => p0.vadd(p)),
			   {'class':'thin'})
	    return
	}

	for(var i = 0; i < this.options['strokes']; i++)
	    p.arc(this.vertex.pt, 2-i*0.2, this.end, this.start, {'class':'angle'})

	if (this.isLabeled)
	{
	    var r = this.option('labelRadius')
	    var lpos = new Ray().bisectrisse(this).unit.scale(r)
		.vadd(this.vertex.pt).vadd([-0.5,-0.5])
	    p.label(this.option('label'),{'at':lpos})
	    
	}
    }

    strokes (n) { return this.option('strokes', n) }
}

const angle = (a,b,c) => new Angle().within(a,b,c)

/*

////////////////////////////////////////////////////////////
// Polygon

class Polygon extends Figure {
    constructor(label, opts) {
	super()
	var defaults = {
	    'labelPosition' : 'automatic'
	}
	this.options = Object.assign(defaults,opts || {})
	this.vertices = []
	this.label = label
    }

    copy(p) {
	this.label  = p.label
	this.options  = p.options
	this.vertices  = p.vertices.map(v => new Point().copy(v))
	return this
    }

    transform (T) {
	this.vertices = this.vertices.map(v => v.transform(T))
	return this
    }
    
    get number () { return this.vertices.length }
	
    get segments () {
	var res = [], vs = this.vertices.concat([this.vertices[0]])
	for(var i = 0; i < this.number; i++)
	    res.push(seg(vs[i],vs[i+1]))
	return res
    }

    vertex (i) { return this.vertices[(this.number + i - 1) % this.number] }

    angle (i) {
	var v1 = this.vertex(i-1)
	var v2 = this.vertex(i)
	var v3 = this.vertex(i+1)
	return new Angle().within(v1,v2,v3)
    }
    
    at(pts) {
	return this.through(pts.map(pt => point(pt)))
    }

    through(ps) {
	this.vertices = ps.map(p => new Point().copy(p))
	return this
    }

    show (p)
    {
	var pts = this.vertices.concat([this.vertices[0]]).map(p => p.pt)
	p.listLinePlot(pts,this.options)
    }

    fill (c) {
	this.options['filled'] = true
	this.options['class'] = c
	return this
    }

    points (f) {
	this.options['points'] = f
	return this
    }

    joined (f) {
	this.options['joined'] = f
	return this
    }   
}

class Triangle extends Polygon {
    constructor (label, opts) {
	super(label, opts)
    }

    side (i) {
	return this.segments[(3+i) % 3]
    }

    median (i) {
	var v = this.vertex(i)
	var s = this.side(i)
	return new Segment().joining(v, point(s.eqn(1/2)))
    }
    
    height (i) {
	var v = this.vertex(i)
	var s = this.side(i)
	return new Segment().through(v).heightTo(s)
    }

    bisectrisse (i) {
	var v = this.vertex(i)
	var a = this.angle(i)
	var s = this.side(i)
	return new Segment().through(v).bisectrisse(a).extendToLine(s)
    }

}

const triangle = (a,b,c) => new Triangle().through([a,b,c])

class Quadrilateral extends Polygon {
    constructor (label, opts) {
	super(label, opts)
    }

    side (i) {
	return this.segments[(i-1) % 4]
    }

    diagonal (i) {
	return seg(vertex(i),vertex(i+2))
    }
}

class Rectangle extends Quadrilateral {
    constructor (label, opts) {
	super(label, opts)
    }

    withSides(a,b) {
	return this.through([point([0,0]),
			     point([a,0]),
			     point([a,b]),
			     point([0,b])])
    }
}
*/

////////////////////////////////////////////////////////////
// predefined objects

const origin = new Point('O').at([0,0])
const horizon = new Line().through(origin,0)
const vertical = new Line().through(origin,90)
const leftBorder = new Line().at([-20,0]).atAngle(90)
const rightBorder = new Line().at([20,0]).atAngle(90)
const topBorder = new Line().at([0,20]).atAngle(0)
const bottomBorder = new Line().at([0,-20]).atAngle(0)
const borders = [leftBorder,rightBorder,topBorder,bottomBorder]

const xRange = [-20,20]
const yRange = [-20,20]

function Chart(f)
{
    this.paper = new Graphics(createSVG(f),{'size':500,'aspectRatio':1,
			   'left-margin':10,'right-margin':10,
			   'top-margin':10,'botom-margin':10,
			   'class':'chart'})
	.xRange(xRange)
    	.yRange(yRange)

    this.put = function (objects)
    {
	objects.flat().forEach(o => o.show(this.paper))
	return this
    }
}
