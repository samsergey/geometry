function Chart(f)
{
    this.paper = new Graphics(createSVG(f),{'size':500,'aspectRatio':1,
			   'left-margin':10,'right-margin':10,
			   'top-margin':10,'botom-margin':10,
			   'class':'chart'})
	.xRange([-20,20])
    	.yRange([-20,20])

    this.put = function (objects)
    {
	objects.flat().forEach(o => o.show(this.paper))
	return this
    }
}


class Point {
    constructor (label,opts)
    {	
	var defaults = {
	    'pointSize' : 3
	}
	
	this.options = Object.assign(defaults,opts || {})
	this.pt = [0,0]
	this.x = 0
	this.y = 0
	this.label = label || ''
	this.label_pt = [-0.5,-0.5]
	this.labelOffset = [0,1]
    }
    
    at ([x,y])
    {
	this.pt = [x,y]
	this.x = x
	this.y = y
	this.label_pt = this.pt.vadd([-0.3*this.label.toString().length,-0.5])
	return this
    }
    
    onLine (line,t)
    {
	this.at(line.eqn(t))
	return this
    }

    between (p1,p2,t)
    {
	return this.onLine(new Segment().joining(p1,p2),t)
    }

    intersection (s1,s2)
    {
	this.at(intersectionL(s1.start,s1.end,s2.start,s2.end))
	return this
    }
 
    setLabel (l)
    {
	this.label = l
	this.label_pt = this.pt.vadd([-0.3*l.toString().length,-0.5])
	return this
    }

    labelOffset (o)
    {
	labelOffset = o.normalize().scale(1.2)
	return this
    }
    
    show (p)
    {
	p.listPlot([this.pt],this.options)
	if (this.label)
	{
	    p.label(this.label,{'at':this.label_pt.vadd(this.labelOffset)})
	}
    }

    transform (T)
    {
    }
}

function point([x,y])
{
    return new Point().at([x,y])
}

function angleV([x,y])
{
    var a = rad2deg(Math.atan(y/x))
    if (x > 0 && y > 0)
	return a
    if (x < 0 && y > 0)
	return 180 + a
    if (x < 0 && y < 0)
	return 180 + a
    if (x > 0 && y < 0)
	return 360 + a
}


// линии (отрезки и лучи) параметризованы в масштабе задающих из точек
// если луч задан точкой и углом, то он имеет единичный масштаб.
const augment = ([x,y]) => [x,y,1]
const pure = ([x,y,_]) => [x,y]
const trans = (T,v) => pure(vmul(T,augment(v)))
const rot = a => [[cos(a),-sin(a),0],[sin(a),cos(a),0],[0,0,1]]
const shift = ([dx,dy]) => [[1,0,dx],[0,1,dy],[0,0,1]]
const flipH = a => [[-1,0,0],[0,1,0],[0,0,1]]
const flipV = a => [[1,0,0],[0,-1,0],[0,0,1]]
const flipC = a => [[-1,0,0],[0,-1,0],[0,0,1]]

class Vector {
    constructor (pt,v)
    {
	this.pivot = pt
	this.vector = v
	this.rebuild()
    }

    rebuild()
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
	this.rebuild()
	return this
    }

    atAngle(a)
    {
	this.vector = [this.length*cos(a),this.length*sin(a)]
	this.rebuild()
	return this
    }

    transformP(T)
    {
	this.pivot = trans(T,this.pivot)
	this.rebuild()
	return this
    }

    transformV(T)
    {
	this.vector = trans(T,this.vector)
	this.rebuild()
	return this
    }

    extend(t1,t2)
    {
	this.pivot = this.eqn(t1)
	this.vector = this.eqn(t2).vsub(this.pivot)
	this.rebuild()
	return this
    }

    shift(d)
    {
	return this.transformP(shift(d))
    }

    rotateV(a)
    {
	return this.transformV(rot(deg2rad(a)))
    }

    rotate(a)
    {
	return this.transformP(rot(deg2rad(a))).transformV(rot(deg2rad(a)))
    }

    rotateAt(p,a)
    {
	var pt = p.pt
	return this.shift(pt.flip).rotate(a).shift(pt)
    }

}

class Segment extends Vector {
    constructor(label,opts) {
	super([0,0],[0,0])
	var defaults = {
	    'arrow' : false,
	    'arrowPosition' : 1
	}
	this.options = Object.assign(defaults,opts || {})
	this.label = label
	this.labelPosition = 1/2
	this.labelOffset = 1.25
    }
  
    joining(p1, p2)
    {
	this.pivot = p1.pt
	this.vector = p2.pt.vsub(this.pivot)
	this.rebuild()
	return this
    }
       
    labelPosition(t)
    {
	this.labelPosition = t
	return this
    }
    
    labelOffset(o)
    {
	this.labelOffset = o
	return this
    }
    
    show(p)
    {
	p.listLinePlot([this.eqn(0), this.eqn(1)], this.options)
	
	if (this.label)
	{
	    var lpos = this.eqn(this.labelPosition).vadd([-0.5,-0.5])
	    var loff = this.norm.scale(this.labelOffset)
	    p.label(this.label,{'at':lpos.vadd(loff)})
	}
	if (this.options['arrow'])
	{
	    var x = this.options['arrowPosition']
	    p.arrow([this.eqn(x - 0.1),this.eqn(x)],this.options)
	}
    }
}


function lineEquation([x1,y1],[x2,y2],s)
{
    var v = [x2-x1,y2-y1], s = s || 1
    return t => [x1,y1].vadd(v.scale(t*s))
}

/*
class Ray extends Segment {

    constructor(label,opts) {
	super(label,opts)
	super.joining(point([0,0]), point([100,0]))
	this.angle = 0
	this.pivot = new Point().at([0,0])
    }

    joining(p1, p2)
    {
	var eqn = lineEquation(p1.pt, p2.pt)
	var s = new Segment().joining(p1, p2)
	this.pivot = p1
	this.start = s.start
	this.end = s.end
	this.eqn = eqn
	this.middle = undefined
	this.vector = s.unit
	this.length = undefined
	this.unit = s.unit
	this.norm = s.norm
	this.angle = s.angle
	this.label_pt = this.pivot.pt.vadd([-0.5,-0.5])
	this.labelOffset = this.norm.scale(1.25)
	return this
    }

    through(p)
    {
	var r = deg2rad(this.angle)
	var p2 = point(p.pt.vadd([cos(r),sin(r)]))
	return this.joining(p,p2) 
    }

    atAngle(a)
    {
	var r = deg2rad(a)
	var p = this.pivot
	var p2 = point(p.pt.vadd([cos(r),sin(r)]))
	return this.joining(p,p2) 
    }

    parallelTo(l)
    {
	return this.atAngle(l.angle)
    }

    orthogonalTo(l)
    {
	return this.atAngle(l.angle+90)
    }

    turn(a)
    {
	return this.atAngle(this.angle+a)
    }

}

class Line extends Ray {

    constructor(label,opts) {
	super(label,opts)
	super.joining(point([0,0]), point([100,0]))
	this.angle = 0
	this.pivot = new Point().at([0,0])
    }

    joining (p1, p2)
    {
	var eqn = lineEquation(p1.pt, p2.pt)
	var s = new Ray().joining(point(eqn(-100)), point(eqn(100)))
	this.pivot = p1
	this.start = s.start
	this.end = s.end
	this.eqn = eqn
	this.middle = undefined
	this.vector = s.unit
	this.length = undefined
	this.unit = s.unit
	this.norm = s.norm
	this.angle = s.angle
	this.label_pt = this.start.vadd([-0.5,-0.5])
	this.labelOffset = this.norm.scale(1.25)
	return this
    }
}

/*
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

function Line(label,opts)
{
    this.joining = function (p1, p2)
    {
	var eqn = lineEquation(p1.pt, p2.pt)
	var res = new Segment(label,opts)
	    .joining(point(eqn(-100)), point(eqn(100)))
	this.eqn = eqn
	this.middle = undefined
	this.vector = this.unit
	this.length = undefined
	return this
    }

    this.through = function (p)
    {
	var r = deg2rad(this.angle)
	var p2 = point(p.pt.vadd([cos(r),sin(r)]))
	return this.joining(p,p2) 
    }

    this.atAngle = function (a)
    {
	var r = deg2rad(a)
	var p = this.eqn(0)
	var p2 = point(p.vadd([cos(r),sin(r)]))
	return this.joining(p,p2) 
    }
}

Object.setPrototypeOf(Line.prototype, Segment.prototype);

function intersection(s1,s2,label,opts)
{
    return new Point(intersectionL(s1.start,s1.end,s2.start,s2.end),label, opts)
}


function angle(s1,s2)
{
    return angleV(s2.unit) - angleV(s1.unit)
}

function Angle(label,opts)
{
    var defaults = {
	'strokes' : 1
    }
    var options = Object.assign(defaults,opts || {})
    this.start = 0
    this.end = 0
    this.vertex = [0,0]
    this.value = 0
    
    this.at = function (p)
    {
	this.vertex = p
	return this
    }

    this.within = function (p1,o,p2)
    {
	var v1 = new Segment().joining(o,p1).unit
	var v2 = new Segment().joining(o,p2).unit
	this.start = angleV(v1)
	this.end = angleV(v2)
	this.vertex = o
	this.value = this.end - this.start
	return this
    }

    this.verticalTo = function (a)
    {
	this.start = a.start + 180
	this.end = a.end + 180
	this.vertex = a.vertex
	this.value = this.end - this.start
	return this
    }

    this.adjacentTo = function (a)
    {
	this.start = a.end
	this.end = a.start + 180
	this.vertex = a.vertex
	this.value = this.end - this.start
	return this
    }

    this.between = function (s1,s2)
    {
	this.start = angleV(s1.unit)
	this.end = angleV(s2.unit)
	this.vertex = point(s1.eqn(0))
	this.value = this.end - this.start
	return this
    }

    this.show = function (p)
    {
	for(var i = 0; i < options['strokes']; i++)
	    p.arc(this.vertex.pt, 2-i*0.2, this.end, this.start, {'class':'angle'})				    
    }
}

function PolyLine(ps,opts)
{
    var defaults = {
	'labelPosition' : [0,-1],
    }
    this.options = Object.assign(defaults,opts || {})

    this.points = ps
    this.show = function (p)
    {
	p.listLinePlot(ps.map(p => p.pt),this.options)
    }   
}

function TriangleV(A,B,C,opts)
{
    
}

function Triangle(a,b,c,opts)
{
    
}

function TriangleR(a,b,opts)
{
    
}


function intersectionL([x11,y11],[x12,y12],[x21,y21],[x22,y22])
{
    var Det = (x12-x11)*y22+(x11-x12)*y21+(x21-x22)*y12+(x22-x21)*y11
    if (Det == 0) return [100,100]
    return [((x12-x11)*x21*y22+(x11-x12)*x22*y21+(x11*x21-x11*x22)*y12+(x12*x22-x12*x21)*y11)/Det, (((x21-x11)*y12+(x12-x21)*y11)*y22+((x11-x22)*y12+(x22-x12)*y11)*y21)/Det]
}


const origin = new Point('O').at([0,0])
const horizon = new Ray().through(origin,0)
const vertical = new Ray().through(origin,90)
*/
