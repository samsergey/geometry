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

class Figure {
    constructor () {  }

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

}

class Point extends Figure {
    constructor (label,opts)
    {
	super()
	var defaults = {
	    'pointSize' : 3
	}
	this.valueOf = 'point'	
	this.options = Object.assign(defaults,opts || {})
	this.pt = [0,0]
	this._label = label || ''
	this._labelOffset = [0,1]
    }

    copy(p) {
	this.pt = p.pt
	if (p._label.length > 0)
	    this._label = p._label + "'"
	return this
    }
    
    transform (T) {
	this.pt = this.trans(T, this.pt)
	return this
    }
    
    static isPoint(obj) {
	return obj.valueOf == 'point'
    }

    get x () { return this.pt[0] }
    get y () { return this.pt[1] }
    get _labelPosition () {
	return this.pt.vadd([-0.3*this._label.toString().length,-0.5])
    }

    get inside () {
	return this.x > xRange[0] && this.x < xRange[1] &&
  	       this.y > yRange[0] && this.y < yRange[1]
    }
    
    at ([x,y])
    {
	this.pt = [x,y]
	return this
    }
    
    onLine (line,t)
    {
	this.at(line.eqn(t))
	return this
    }

    start (s) { return this.onLine(s,0) }
    middle (s) { return this.onLine(s,1/2) }
    end (s) { return this.onLine(s,1) }
    
    between (p1,p2,t)
    {
	return this.onLine(new Segment().joining(p1,p2),t)
    }

    intersection (s1,s2)
    {
	if (!(Vector.isVector(s1) && Vector.isVector(s2)))
	    console.error('intersection of non vectors')
	this.at(intersectionL(s1.start.pt,s1.end.pt,s2.start.pt,s2.end.pt))
	return this
    }

    azimuth (p1,a1,p2,a2)
    {
	var s = new Segment().joining(p1,p2)
	var s1 = new Segment().through(p1).atAngle(s.angle + a1)
	var s2 = new Segment().through(p2).atAngle(s.angle + 180 - a2)
	return this.intersection (s1,s2)
    }

    
    label (l)
    {
	this._label = l
	return this
    }

    labelPosition (o)
    {
	this._labelOffset = o.normalize().scale(1.2)
	return this
    }
    
    show (p)
    {
	p.listPlot([this.pt],this.options)
	if (this._label)
	{
	    p.label(this._label,{'at':this._labelPosition.vadd(this._labelOffset)})
	}
    }

}

function point([x,y])
{
    return new Point().at([x,y])
}

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

class Vector extends Figure {
    constructor (pt,v)
    {
	super()
	this.valueOf = 'vector'
	this.pivot = pt
	this.vector = v
	this.reset()
    }

    static isVector (obj) {
	return obj.valueOf == 'vector'
    }

    copy(v) {
	this.pivot = v.pivot
	this.vector = v.vector
	this.reset()
	return this
    }

    
    transform (T) {
	this.pivot = this.trans(T, this.pivot)
	var e = this.trans(T, this.end)
	this.vector = this.e.vsub(this.pivot)
	return this
    }
    
    reset()
    {
	this.eqn = lineEquation(this.pivot, this.pivot.vadd(this.vector))
    }
    
    get start () { return point(this.eqn(0)) }
    get end () { return point(this.eqn(1)) } 
    get unit () { return this.vector.normalize() }
    get length () { return this.vector.norm() } 
    get norm () { return [-this.unit[1],this.unit[0]] } 
    get angle () { return angleV(this.unit) }

    at(p)
    {
	this.pivot = p
	this.reset()
	return this
    }

    location (p)
    {
	return p.vsub(this.pivot).dot(this.unit)/this.length
    }
    
    atAngle(a)
    {
	var r = deg2rad(a)
	this.vector = [cos(r), sin(r)].scale(this.length)
	this.reset()
	return this
    }

    extend(t1,t2)
    {
	this.pivot = this.eqn(t1)
	this.vector = this.eqn(t2).vsub(this.pivot)
	this.reset()
	return this
    }

    intersection (v)
    {
	return intersectionL(this.pivot,this.pivot.vadd(this.vector),
			     v.pivot,v.pivot.vadd(v.vector))
    }
}

class Segment extends Vector {
    constructor(label,opts) {
	super([0,0],[1,0])
	var defaults = {
	    'arrow' : false,
	    'arrowPosition' : 1,
	    'mark' : 0
	}
	this.options = Object.assign(defaults,opts || {})
	this._label = label
	this._labelPosition = 1/2
	this._labelOffset = 1.25
    }

    copy(s) {
	this.pivot = s.pt
	this.vector = s.vector
	this.reset()
	return this
    }

    joining(p1, p2)
    {
	this.pivot = p1.pt
	this.vector = p2.pt.vsub(this.pivot)
	this.reset()
	return this
    }

    intersectionPoint (v)
    {
	return new Point().intersection(this, v)	
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
    
    extendToLine(v)
    {
	return this.joining(this.start, this.intersectionPoint(v))
    }

    extendToLength(a)
    {
	var p = this.pivot.vadd(this.unit.scale(a))
	return this.joining(this.start, point(p))
    }

    bisectrisse (an)
    {
	return this.atAngle((an.start + an.end)/2)
    }

    heightTo(l)
    {
	return this.perpendicularTo(l).extendToLine(l)
    }
    
    label(l)
    {
	this._label = l
	return this
    }

    
    labelPosition(t)
    {
	this._labelPosition = t
	return this
    }
    
    labelOffset(o)
    {
	this._labelOffset = o
	return this
    }

    show(p)
    {
	p.listLinePlot([this.eqn(0), this.eqn(1)], this.options)
	
	if (this._label)
	{
	    var lpos = this.eqn(this._labelPosition).vadd([-0.5,-0.5])
	    var loff = this.norm.scale(this._labelOffset)
	    p.label(this._label,{'at':lpos.vadd(loff)})
	}
	if (this.options['arrow'])
	{
	    var x = this.options['arrowPosition']
	    p.arrow([this.eqn(x - 0.1),this.eqn(x)],this.options)
	}
	if (this.options['mark'] > 0)
	{
	    for (var i = 0; i < this.options['mark']; i++) {
		var m = this.eqn(1/2).vadd(this.unit.scale(0.2*i))
		p.line([m.vadd(this.norm.scale(0.3)),
			m.vadd(this.norm.scale(-0.3))],
		       {'class':'thin'})
	    }
	}
    }

    mark (n)
    {
	this.options['mark'] = n
	return this
    }
    
    dashed ()
    {
	this.options['class'] = 'dashed'
	return this
    }

    thin ()
    {
	this.options['class'] = 'thin'
	return this
    }

    thick ()
    {
	this.options['class'] = 'thick'
	return this
    }

    red ()
    {
	this.options['class'] = 'red'
	return this
    }

}

const seg = (a,b) => new Segment().joining(a,b)

function lineEquation([x1,y1],[x2,y2],s)
{
    var v = [x2-x1,y2-y1], s = s || 1
    return t => [x1,y1].vadd(v.scale(t*s))
}

class Ray extends Segment {

    constructor(label,opts) {
	super(label,opts)
	this.vector = [1,0]
    } 
  
    flip () {
	return this.atAngle(this.angle+180)
    }
  
    show (p) {
	var t = borders.map(b => this.location(this.intersection(b)))
	    .filter(x => x > 0).min()
	
	p.listLinePlot([this.eqn(0), this.eqn(t)], this.options)
	
	if (this._label)
	{
	    var lpos = this.eqn(this._labelPosition).vadd([-0.5,-0.5])
	    var loff = this.norm.scale(this._labelOffset)
	    p.label(this._label,{'at':lpos.vadd(loff)})
	}
	if (this.options['arrow'])
	{
	    var x = this.options['arrowPosition']
	    p.arrow([this.eqn(x - 0.1),this.eqn(x)],this.options)
	}
    }
}

const ray = (a,b) => new Ray().joining(a,b)

class Line extends Ray {

    constructor(label,opts) {
	super(label,opts)
    }
   
    show(p)
    {
	var ts = borders.map(b => this.location(this.intersection(b)))
	var t2 = ts.filter(x => x > 0).min()
	var t1 = ts.filter(x => x < 0).max()

	p.listLinePlot([this.eqn(t1), this.eqn(t2)], this.options)
	
	if (this._label)
	{
	    var lpos = this.eqn(this._labelPosition).vadd([-0.5,-0.5])
	    var loff = this.norm.scale(this._labelOffset)
	    p.label(this._label,{'at':lpos.vadd(loff)})
	}
	if (this.options['arrow'])
	{
	    var x = this.options['arrowPosition']
	    p.arrow([this.eqn(x - 0.1),this.eqn(x)],this.options)
	}
    }
}

const line = (a,b) => new Line().joining(a,b)

class Angle extends Figure {
    constructor (label,opts) {
	super()
	var defaults = {
	    'strokes' : 1,
	    'labelRadius' : 3
	}
	this.options = Object.assign(defaults,opts || {})
	this.start = 0
	this.end = 0
	this.vertex = origin
	this.label = label
    }

    copy(an) {
	this.start = an.start
	this.end = an.end
	this.vertex = an.vertex
	return this
    }

    transform (T) {
	this.vertex = this.vertex.transform(T)
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
	this.vertex = a.vertex
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
	var v1 = new Segment().joining(o,p1)
	var v2 = new Segment().joining(o,p2)
	this.start = v1.angle
	this.end = v2.angle
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
	if (this.label)
	{
	    var r = this.options['labelRadius']
	    var lpos = new Ray().bisectrisse(this).unit.scale(r)
		.vadd(this.vertex.pt).vadd([-0.5,-0.5])
	    p.label(this.label,{'at':lpos})
	    
	}
    }

    strokes (n) {
	this.options.strokes = n
	return this
    }
}

const angle = (a,b,c) => new Angle().within(a,b,c)

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
	pts = this.vertices.concat([this.vertices[0]]).map(p => p.pt)
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

var pts

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

function intersectionL([x11,y11],[x12,y12],[x21,y21],[x22,y22])
{
    var Det = (x12-x11)*y22+(x11-x12)*y21+(x21-x22)*y12+(x22-x21)*y11
    if (Det == 0) return [100,100]
    return [((x12-x11)*x21*y22+(x11-x12)*x22*y21+(x11*x21-x11*x22)*y12+(x12*x22-x12*x21)*y11)/Det, (((x21-x11)*y12+(x12-x21)*y11)*y22+((x11-x22)*y12+(x22-x12)*y11)*y21)/Det]
}


const origin = new Point('O').at([0,0])
const horizon = new Line().through(origin,0)
const vertical = new Line().through(origin,90)
const leftBorder = new Line().at([-20,0]).atAngle(90)
const rightBorder = new Line().at([20,0]).atAngle(90)
const topBorder = new Line().at([0,20]).atAngle(0)
const bottomBorder = new Line().at([0,-20]).atAngle(0)
const borders = [leftBorder,rightBorder,topBorder,bottomBorder]


function CoordRay(p,n,opts)
{
    var defaults = {
	'labelPosition' : [0,-1],
	'stepLength':1
    }

    this.options = Object.assign(defaults,opts || {})
    var O = p.labelOffset(this.options['labelPosition'])
    var r = new Ray('',{'arrow':false,
			'arrowPosition':n+1.5})
	.through(O,0)
	.setScale(this.options['stepLength'])
    var Ps = range(0,n+1).map(i => new Point(i)
			      .onLine(r, i)
			      .labelOffset(this.options['labelPosition']))
    return [O, r, Ps].flat()
}

// example
// CoordRay(new Point([-18,1]),17,{stepLength:2, labelPosition:[0,1]})
