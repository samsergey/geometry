////////////////////////////////////////////////////////////
// math

function angleV([x,y]) {
    // tested
    if (x == 0 && y == 0) return 0;
    if (x == 1 && y == 0) return 0;
    if (x == 0 && y == 1) return 90;
    if (x == -1 && y == 0) return 180;
    if (x == 0 && y == -1) return 270;
    var a = rad2deg(Math.atan(y/x));
    if (x >= 0 && y >= 0) return a;
    if (x <= 0 && y >= 0) return 180 + a;
    if (x <= 0 && y < 0) return 180 + a;
    if (x > 0 && y <= 0) return 360 + a;
    return a;
}

function lineEquation([x1, y1], [x2, y2], s) {
    // tested
    var v = [x2 - x1, y2 - y1], s = s || 1;
    return t => [x1, y1].vadd(v.scale(t*s));
}

function intersectionV([x1, y1], [v1x, v1y], [x2, y2], [v2x, v2y]) {
    // tested
    var D = v1y*v2x - v1x*v2y;
    var D1 = v1x*y1 - v1y*x1;
    var D2 = v2y*x2 - v2x*y2;
    if (D == 0) return [1/0, 1/0];
    return [-(v1x*D2 + v2x*D1)/D, -(v1y*D2 + v2y*D1)/D];
}

function cross([x1, y1], [x2, y2]) {
    return x1*y2 - x2*y1;
}

////////////////////////////////////////////////////////////
// Figure and transformations

class Figure {
    constructor () {
	this.options = {};
    }

    get isCurve () { return false; }
    
    augment ([x, y]) { return [x, y, 1]; }
    pure ([x, y, _]) { return [x, y]; }
    trans (T, v) { return this.pure(vmul(T, this.augment(v))); }
    rotT (a) { return [[cos(a), -sin(a), 0],
		       [sin(a), cos(a), 0],
		       [0, 0, 1]]; }
    reflectT (a) { return [[cos(2*a), sin(2*a), 0],
			   [sin(2*a), -cos(2*a), 0],
			   [0, 0, 1]]; }
    shiftT ([dx, dy]) { return [[1, 0, dx], [0, 1, dy], [0, 0, 1]]; }
    scaleT (a, b) { return [[a, 0, 0], [0, b, 0], [0, 0, 1]]; }
    flipHT (a) { return [[-1, 0, 0], [0, 1, 0], [0, 0, 1]]; }
    flipVT (a) { return [[1, 0, 0], [0, -1, 0], [0, 0, 1]]; }
    flipCT (a) { return [[-1, 0, 0], [0, -1, 0], [0, 0, 1]]; }
    
    transform (T) {
	console.error("transform is unimplemented");
    }

    shift(d) {	return this.transform(this.shiftT(d)); }
    
    scale(a, b) {
	return this.transform(this.scaleT(a, b||a));
    }

    scaleAt(p, a, b) {
	var pt = p.pt;
	return this.shift(pt.flip()).scale(a, b).shift(pt);
    }
    
    superpose(p1, p2) {
	return this.shift(line(p1, p2).vector);
    }
    
    rotate(a) {
	return this.transform(this.rotT(deg2rad(a)));
    }

    rotateAt(p, a) {
	if (p instanceof Point) {
	    var pt = p.pt;
	    return this.shift(pt.flip()).rotate(a).shift(pt);
	}
	console.error('rotation could be done against a Point');
	return this;
    }

    reflect (a) {
	return this.transform(this.reflectT(deg2rad(a)));
    }
    
    reflectAt(that, a) {
	if (that instanceof Point) {
	    var l = new Line().through(that).atAngle(a)
	    return this.reflectAt(l);
	}
	if (that instanceof Line) {
	    var b = that.angle;
	    var pt = that.pivot;
	    return this.shift(pt.flip()).reflect(b).shift(pt);
	}
	console.error('reflection could be done against Point or Line');
	return this;
    }

    option (opt, x) {
	if (x) {
	    this.options[opt] = x;
	    return this;
	}
	else
	    return this.options[opt];
    }

    style (c) { return this.option('class', c); }

    get isLabeled () { return !!this.options['label']; }
    named (l) { return this.option('label', l); }

    isEqual (that) { return 'unimplemented'; }
    isSimilar (that) { return 'unimplemented'; }
}

////////////////////////////////////////////////////////////
// Group

class Group extends Figure {
    constructor(fs) {
	super();
	this.contents = fs;
    }

    copy () {
	var res = new Group();
	res.contents = this.contents.map(f => f.copy());
	return res;
    }

    transform (T) {
	this.contents.forEach(f => f.transform(T));
	return this;
    }

    element (i) {
	return this.contents[(i - 1) % this.contents.length];
    }
    
    show (p)
    {
	this.contents.forEach(f => f.show(p));
	return this;
    }
}

////////////////////////////////////////////////////////////
// Point

class Point extends Figure {
    constructor (label)
    {
	super();
	var defaults = {
	    'pointSize' : 4, 
	    'label' : label, 
	    'labelOffset': [0, 1], 
	    'class' : 'regular'
	};
	this.options = Object.assign(defaults, super.options);
	this.valueOf = 'point' ;
	this.pt = [0, 0];
    }

    static isPoint(obj) { return obj.valueOf == 'point'; }

    isEqual (that) {
	return (that instanceof Point) &&
	    equal(this.pt, that.pt);
    }

    copy() {
	var res = new Point();
	res.pt = this.pt.copy();
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }
    
    transform (T) {
	this.pt = this.trans(T, this.pt);
	return this;
    }

    get x () { return this.pt[0]; }
    get y () { return this.pt[1]; }

    get isInside () {
	return this.x > xRange[0] && this.x < xRange[1] &&
 	    this.y > yRange[0] && this.y < yRange[1];
    }
    
    at ([x, y]) {
	this.pt = [x, y];
	return this;
    }
    
    on (curve, t) { return this.at(curve.eqn(t)); }

    distance (that) {
	if (Point.isPoint(that))
	    return seg(this, that).length;
	if (Line.isLine(that))
	    return new Segment().through(this).heightTo(that).length;
	if (Circle.isCircle(that))
	    return abs(this.distance(that.center)-that.radius);
	return [];
    }

    between (p1, p2, t) {
	return this.on(new Line().joining(p1, p2), t);
    }

    intersection (s1, s2, n) {
	if (!(s1.isCurve && s1.isCurve))
	    console.error('intersection of non vectors');
	this.pt = s1.intersection(s2)[n || 0].pt;
	return this;
    }

    azimuth (p1, a1, p2, a2) {
	var s = new Line().joining(p1, p2);
	var s1 = new Line().through(p1).atAngle(s.angle + a1);
	var s2 = new Line().through(p2).atAngle(s.angle + 180 - a2);
	return s1.intersection(s2)[0];
    }
    
    label (l) {
	this.option('label', l);
	return this;
    }

    labelOffset (o) {
	this.option('labelOffset', o.normalize().scale(1.2));
	return this;
    }
    
    show (p) {
	if (this.isInside)
	    p.listPlot([this.pt], this.options);
	
	if (this.isLabeled) {
	    var l = this.option('label');
	    var lp = this.pt.vadd([-0.3*l.length, -0.5]);
	    p.label(l, {'at' : lp.vadd(this.option('labelOffset'))});
	}
    }
}

const point = ([x, y]) => new Point().at([x, y]);

////////////////////////////////////////////////////////////
// Line

class Line extends Figure {
    constructor(label) {
	super();
	this.options = {
	    'arrow' : false, 
	    'arrowPosition' : 1, 
	    'label' : label, 
	    'labelPosition' : 5, 
	    'labelOffset' : 1.25, 
	    'class' : 'regular'
	};
	this.valueOf = 'line';
	this.pivot = [0, 0];
	this.vector = [1, 0];
	this.eqn = t => [t, 0];
	this.resetEqn();
    }

    static isLine (obj) { return obj.valueOf == 'line'; }
    get isCurve () { return true; }

    copy() {
	var res = new Line();
	res.pivot = this.pivot.copy();
	res.vector = this.vector.copy();
	res.resetEqn();
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }
    
    transform (T) {
	this.pivot = this.trans(T, this.pivot);
	var e = this.trans(T, this.eqn(1));
	this.vector = e.vsub(this.pivot);
	return this.resetEqn();
    }

    rescale (s) {
	this.vector = this.vector.scale(s);
	return this.resetEqn();
    }
    
    get unit () { return this.vector.normalize(); }
    get norm () { return [-this.unit[1], this.unit[0]]; } 
    get middle () { return this.eqn(1/2); } 
    get angle () { return angleV(this.unit); }

    resetEqn() {
	this.eqn = lineEquation(this.pivot, this.pivot.vadd(this.vector));
	return this;
    }

    contains (p) {
	return abs(cross(p.pt.vsub(this.pivot), this.unit)) < 1e-13;
    }
    
    isParallelTo (l) { return abs (l.angle - this.angle) % 180 == 0; }

    joining(p1, p2) {
	this.pivot = p1.pt;
	this.vector = p2.pt.vsub(this.pivot);
	this.resetEqn();
	return this;
    }
    
    at(p) {
	this.pivot = p;
	this.resetEqn();
	return this;
    }

    location (p) {
	return p.vsub(this.pivot).dot(this.unit)/this.vector.norm();
    }
    
    atAngle(a) {
	var r = deg2rad(a);
	this.vector = [cos(r), sin(r)];
	this.resetEqn();
	return this;
    }
    
    intersection (that) {
	if (Line.isLine(that))
	    return [point(intersectionV(this.pivot, this.unit, that.pivot, that.unit))];
	if (Circle.isCircle(that))
	    return that.intersectionL(this);
	if (Polygon.isPolygon(that))
	    return that.intersection(this);
	return [];
    }
    
    through (p) { return this.at(p.pt); }
    parallelTo (l) { return this.atAngle(l.angle); }
    along (l) { return this.parallelTo(l); }
    perpendicularTo (l) { return this.atAngle(l.angle+90); }
    bisectrisse (an) { return this.atAngle((an.start + an.end)/2); }
    midPerpendicular (s) { return this.through(point(s.eqn(1/2))).perpendicularTo(s); }

    tangentTo(c, n) {
	if (point(this.pivot).distance(c.center) < c.radius) {
	    console.error("The pivot point of the tangent line is inside the circle!");
	    return this;
	}
	var d = seg(point(this.pivot), c.center);
	var n = n || 0;
	var a = (n==0 ? -1 : 1) * rad2deg(Math.asin(c.radius/d.length));
	return this.atAngle(d.angle+a);
    }
    
    distance (that) {
	if (Point.isPoint(that))
	    return that.distance(this);
	if (Circle.isCircle(that))
	    return abs(this.distance(that.center) - that.radius);
	else
	    console.error('object must be a point or a circle!');
	return 0;
    }

    isEqual (that) {
	return (that instanceof Line) &&
	    equal(this.pivot, that.pivot) &&
	    equal(this.vector, that.vector); 
    }
    
    show(p) {
	var ts = borders.map(b => this.location(this.intersection(b)[0].pt));
	var t2 = ts.filter(x => x > 0).min();
	var t1 = ts.filter(x => x < 0).max();

	p.listLinePlot([this.eqn(t1), this.eqn(t2)], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).vsub(this.unit.scale(2));
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    p.label(this.option('label'), {'at' : lpos.vadd(loff)});
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    p.arrow([this.eqn(x - 0.1), this.eqn(x)], this.options);
	}
    }
}

const line = (a, b) => new Line().joining(a, b);

class Ray extends Line {

    constructor(label) {
	super(label);
	var defaults = {
	    'class' : 'regular'
	};
	this.options = Object.assign(defaults, super.options);
    } 

    copy() {
	var res = new Ray().joining(point(this.eqn(0)),point(this.eqn(1)));
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }
    
    flip () {
	return this.atAngle(this.angle+180);
    }

    contains (p) {
	return super.contains(p) && this.location(p.pt) >= 0;
    }
    
    isEqual (that) {
	return (that instanceof Ray) &&
	    super.isEqual(that);
    }

    show(p) {
	var ts = borders.map(b => this.location(this.intersection(b)[0].pt));
	var t2 = ts.filter(x => x > 0).min();

	p.listLinePlot([this.eqn(0), this.eqn(t2)], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).vsub(this.unit.scale(2));
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    p.label(this.option('label'), {'at' : lpos.vadd(loff)});
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    p.arrow([this.eqn(x - 0.1), this.eqn(x)], this.options);
	}
    }
}

const ray = (a, b) => new Ray().joining(a, b);

class Segment extends Line {
    constructor(label) {
	super(label);
	var defaults = {
	    'mark' : 0, 
	    'label': label, 
	    'labelPosition' : 1/2, 
	    'labelOffset' : 1.25, 
	    'class' : 'regular'
	};
	this.options = Object.assign(defaults, super.options);
    }

    copy() {
	var res = new Segment().joining(this.start, this.end);
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }

    get start () { return point(this.eqn(0)); }
    get end () { return point(this.eqn(1)); } 
    get length () { return this.vector.norm(); } 

    contains (p) {
	return super.contains(p)
	    && this.location(p.pt) >= 0
	    && this.location(p.pt) <= 1;
    }

    extend(t1, t2)
    {
	this.pivot = this.eqn(t1);
	this.vector = this.eqn(t2).vsub(this.pivot);
	this.resetEqn();
	return this;
    }
    
    extendToLine(v)
    {
	return this.joining(this.start, v.intersection(this)[0]);
    }

    extendToLength(a)
    {
	var p = this.pivot.vadd(this.unit.scale(a));
	return this.joining(this.start, point(p));
    }

    heightTo(l)
    {
	return this.perpendicularTo(l).extendToLine(l);
    }

    tangentTo(c, n) {
	var t = super.tangentTo(c, n);
	if (c.contains(point(this.pivot))) {
	    return this.parallelTo(t);
	} else {
	    var d = point(this.pivot).distance(c.center);
	    var p = this.extendToLength(sqrt(d**2-c.radius**2)).end;
	    var a = c.location(p);
	    return this.joining(point(this.pivot), point(c.eqn(a)));
	}
    }
    
    intersection (that) {
	return super.intersection(that).filter(p => this.contains(p));
    }

    isEqual (that) {
	return (that instanceof Segment) &&
	    super.isEqual(that);
    }

    show(p)
    {
	p.listLinePlot([this.eqn(0), this.eqn(1)], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(this.option('labelPosition'));
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    p.label(this.option('label'), {'at' : lpos.vadd(loff)});
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    p.arrow([this.eqn(x - 0.1), this.eqn(x)], this.options);
	}
	if (this.option('mark') > 0)
	{
	    for (var i = 0; i < this.option('mark'); i++) {
		var m = this.eqn(1/2).vadd(this.unit.scale(0.2*i));
		p.line([m.vadd(this.norm.scale(0.3)), 
			m.vadd(this.norm.scale(-0.3))], 
		       {'class':'thin'});
	    }
	}
    }

    mark (n) { return this.option('mark', n); }
    arrow () { return this.option('arrow', true); }

}

const seg = (a, b) => new Segment().joining(a, b);


////////////////////////////////////////////////////////////
// Angle

class Angle extends Figure {
    constructor (value) {
	super();
	var defaults = {
	    'strokes' : 1, 
	    'labelRadius' : 3, 
	    'label' : 'value'
	};
	this.options = Object.assign(defaults, super.options);
	this.start = 0;
	this.end = value;
	this.vertex = [0, 0];
	this.valueOf = 'angle';
    }

    static isAngle(obj) { return obj.valueOf == 'angle'; }
    
    copy() {
	var res = new Angle();
	res.start = this.start;
	res.end = this.end;
	res.vertex = this.vertex.copy();
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return this;
    }

    on (l) {
	var v = this.value
	this.vertex = point(l.pivot)
	this.start = l.angle
	this.end = this.start + v
	return this
    }
    
    transform (T) {
	this.vertex = this.vertex.transform(T);
	this.start = this.startRay.transform(T).angle();
	this.end = this.endRay.transform(T).angle();
	return this;
    }
    
    get value () { return (this.end - this.start) % 360; }
    get startRay () {
	return new Ray().through(this.vertex).atAngle(this.start);
    }
    get endRay () {
	return new Ray().through(this.vertex).atAngle(this.end);
    }
    get isRight () {
	return this.value == 90 || cos(deg2rad(this.value)) < 1e-14;
    }
    
    atPoint (p) {
	this.vertex = p;
	return this;
    }
    
    vertical () {
	this.start = (this.start + 180) % 360;
	this.end = (this.end + 180) % 360;
    }

    adjacent () {
	[this.start, this.end] = [this.end, (this.start + 180) % 360];
	return this;
    }

    complement () {
	[this.start, this.end] = [this.end, this.start];
	return this;
    }

    equalTo (a) {
	this.start = a.start;
	this.end = a.end;
	return this;
    }

    verticalTo (a) { return this.equalTo(a).vertical(); }
    adjacentTo (a) { return this.equalTo(a).adjacent(); }
    complementTo (a) { return this.equalTo(a).complement(); }
    
    within (p1, o, p2) {
	this.start = ray(o, p1).angle;
	this.end = ray(o, p2).angle;
	this.vertex = o;
	return this;
    }
    
    between (s1, s2) {
	this.start = s1.angle;
	this.end = s2.angle;
	this.vertex = point(s1.eqn(0));
	return this;
    }

    isEqual (that) {
	return (that instanceof Angle) &&
	    equalMod(360, this.start, that.start) &&
	    equalMod(360, this.end, that.end);
    }
    
    show (p) {
	if (this.isRight)
	{
	    var p0 = this.vertex.pt;
	    var p1 = this.startRay.unit;
	    var p2 = this.endRay.unit;
	    p.listLinePlot([p1, p1.vadd(p2), p2].map(p => p0.vadd(p)), 
			   {'class':'angle'});
	    return;
	}

	for(var i = 0; i < this.options['strokes']; i++)
	    p.arc(this.vertex.pt, 1-i*0.2, this.end, this.start, {'class':'angle'});

	if (this.isLabeled)
	{
	    var l = this.option('label')
	    if (l == 'value')
		l = abs(round(this.value))+'°';
	    var r = this.option('labelRadius');
	    var lpos = new Ray().bisectrisse(this).unit.scale(r)
		.vadd(this.vertex.pt).vadd([-0.5,-0.5]);
	    p.label(l,{'at':lpos});
	    
	}
    }

    strokes (n) { return this.option('strokes', n); }
}

const angle = (a,b,c) => new Angle().within(a,b,c);

////////////////////////////////////////////////////////////
// Polygon

class Polygon extends Figure {
    constructor(label) {
	super();
	var defaults = {
	    'labelPosition' : 'automatic',
	    'class':'regular',
	    'points' : true,
	    'label':label,
	    'segmentLabels':[]
	};
	this.valueOf = 'polygon';
	this.options = Object.assign(defaults,super.options);
	this.vertices = [];
    }

    static isPolygon(obj) { return obj.valueOf == 'polygon'; }
    get isCurve () { return true; }

    copy() {
	var res = new Polygon();
	res.vertices = this.vertices.map(v => v.copy());
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }
    
    transform (T) {
	this.vertices = this.vertices.map(v => v.transform(T));
	return this;
    }
    
    get number () { return this.vertices.length; }
    get segments () {
	var res = [], vs = this.vertices.concat([this.vertices[0]]);
	for(var i = 0; i < this.number; i++) {
	    var s = this.options.segmentLabels[i];
	    res.push(new Segment(s).joining(vs[i], vs[i+1]));
	}
	return res;
    }
    
    vertex (i) { return this.vertices[(this.number + i - 1) % this.number]; }

    labelVertices (ls) {
	this.vertices.map((v,i) => v.label(l[i]));
	return this;
    }
    
    segmentLabels (ls) { return this.option('segmentLabels',ls); }

    angle (i) {
	var v1 = this.vertex(i-1);
	var v2 = this.vertex(i);
	var v3 = this.vertex(i+1);
	return new Angle().within(v1,v2,v3);
    }

    contains (p) { return this.segments.some(s => s.contains(p)); }
    
    through(ps) {
	this.vertices = ps;
	return this;
    }

    intersection (that) { return this.segments.mapappend(s => s.intersection(that)); }

    isEqual (that) {
	return (that instanceof Polygon) &&
	    equal(this.vertices, that.vertices);
    }
    
    show (p)
    {
	this.segments.forEach(s => s.show(p));
	if (this.option('points'))
	    this.vertices.forEach(s => s.show(p));
    }

    fill (c) {
	this.options['filled'] = true;
	this.options['class'] = c;
	return this;
    }

    points (f) {
	this.options['points'] = f;
	return this;
    }

    joined (f) {
	this.options['joined'] = f;
	return this;
    }  
}

class Triangle extends Polygon {
    constructor (label) {
	super(label);
    }

    side (i) { return this.segments[(3+i) % 3]; }

    get sides () { return this.segments; }
    
    median (i) {
	var v = this.vertex(i);
	var s = this.side(i);
	return new Segment().joining(v, point(s.eqn(1/2)));
    }
    
    height (i) {
	var v = this.vertex(i);
	var s = this.side(i);
	return new Segment().through(v).heightTo(s);
    }

    bisectrisse (i) {
	var v = this.vertex(i);
	var a = this.angle(i);
	var s = this.side(i);
	return new Segment().through(v).bisectrisse(a).extendToLine(s);
    }

}

const triangle = (a,b,c) => new Triangle().through([a,b,c]);

class Quadrilateral extends Polygon {
    constructor (label) {
	super(label);
    }
    
    side (i) { return this.segments[(i-1) % 4]; }

    diagonal (i) { return seg(this.vertex(i),this.vertex(i+2)); }
}

class Rectangle extends Quadrilateral {
    constructor (label) {
	super(label);
    }

    withSides(a,b) {
	return this.through([point([0,0]),
			     point([0,b]),
			     point([a,b]),
			     point([a,0])])
	    .shift([-a/2,-b/2]);
    }

    on(s,b) {
	var p1 = s.start;
	var p2 = s.end;
	var p3 = new Segment().through(p2).perpendicularTo(s).extendToLength(b).end;
	var p4 = new Segment().through(p1).perpendicularTo(s).extendToLength(b).end;
	return this.through([p1,p2,p3,p4]);
    }
}

class Square extends Quadrilateral {
    constructor (label) {
	super(label);
    }

    withSide(a) {
	return this.through([point([0,0]),
			     point([0,a]),
			     point([a,a]),
			     point([a,0])])
	    .shift([-a/2,-a/2]);
    }

    on(s) {
	var a = s.length;
	var p1 = s.start;
	var p2 = s.end;
	var p3 = new Segment().through(p2).perpendicularTo(s).extendToLength(a).end;
	var p4 = new Segment().through(p1).perpendicularTo(s).extendToLength(a).end;
	return this.through([p1,p2,p3,p4]);
    }
}

////////////////////////////////////////////////////////////
// Circle

class Circle extends Figure {
    constructor(r) {
	super();
	var defaults = {
	    'class':'regular',
	};
	this.valueOf = 'circle';
	this.options = Object.assign(defaults,super.options);
	this.center = origin.copy();
	this.radius = r || 1;
	this.resetEqn();
    }

    static isCircle(obj) { return obj.valueOf == 'circle'; }
    get isCurve () { return true; }

    resetEqn() {
	var c = this.center.pt;
	var r = this.radius;
	this.eqn = t => [c[0]+r*cos(t),c[1]+r*sin(t)];
	return this;
    }

    location (p) {
	return deg2rad(line(this.center, p).angle);
    }
    
    copy() {
	var res = new Circle();
	res.center = this.center.copy();
	res.radius = this.radius;
	Object.assign(res.options, this.options);
	return res.resetEqn();
    }
    
    transform (T) {
	var x = new Segment().through(this.center).extendToLength(this.radius);
	this.center.transform(T);
	return this.through1(x.end.transform(T));
    }

    at(p) {
	this.center = p;
	return this.resetEqn();
    }

    contains (p) {
	return abs(p.distance(this.center) - this.radius) < 1e-14;
    }
    
    intersectionC (that) {
	var r0 = this.radius, r1 = that.radius;
	var r = seg(this.center, that.center);
	var R = r.length;
	if (R == 0)
	    return [];
	if (R > r0 + r1 || R < abs(r0-r1))
	    return [];
	var a = (r0**2 - r1**2)/(2*R);
	var b = 1/2*sqrt(2*(r0**2 + r1**2) - 4*a**2 - R**2);
	var m = r.middle.vadd(r.unit.scale(a));
	if (b == 0)
	    return [point(m.vadd(r.norm.scale(b)))];
	else 
	    return [point(m.vadd(r.norm.scale(b))), point(m.vadd(r.norm.scale(-b)))];
    }

    intersectionL (that) {
	var h = new Segment().through(this.center).heightTo(that);
	if (h.length > this.radius)
	    return [];
	if (h.length == this.radius)
	    return [h.end];
	var x = sqrt(this.radius**2 - h.length**2);
	var s = new Segment().through(h.end).along(that).extend(-x,x);
	return [s.start, s.end];
    }

    intersection (that) {
	if (Line.isLine(that))
	    return this.intersectionL(that);
	if (Circle.isCircle(that))
	    return this.intersectionC(that);
	if (Polygon.isPolygon(that))
	    return that.intersection(this);
	return [];
    }
    
    through1 (p) {
	this.radius = seg(this.center, p).length;
	return this.resetEqn();
    }

    through2 (p1, p2) {
	var d = p1.distance(p2);
	if (d > this.radius*2) {
	    this.radius = d/2;
	    this.center = new Point().middle(seg(p1,p2));
	} else {
	    var c1 = new Circle(this.radius).at(p1);
	    var c2 = new Circle(this.radius).at(p2);
	    this.center = c1.intersectionC(c2)[0];
	}
	return this.resetEqn();
    }

    through3 (p1,p2,p3) {
	var l1 = new Line().midPerpendicular(seg(p1,p2));
	var l2 = new Line().midPerpendicular(seg(p2,p3));
	if (l1.isParallelTo(l2))
	    console.error("Can't build a circle, three points are aligned.");
	this.center = new Point().intersection(l1,l2)[0];
	this.radius = this.center.distance(p1);
	return this.resetEqn();
    }

    through (p1,p2,p3) {
	if (!p2 && !p3)
	    return this.through1(p1);
	else if (!p3)
	    return this.through2(p1,p2);
	else
	    return this.through3(p1,p2,p3);
    }

    distance (that) {
	if (Point.isPoint(that) || Line.isLine(that))
	    return that.distance(this);
	else if (Circle.isCircle(that))
	    return abs(this.distance(that.center) - that.radius - this.radius);
	else
	    console.error('object must be a point or a circle!');
	return 0;
    }

    isEqual (that) {
	return (that instanceof Circle) &&
	    equal(this.center, that.center) &&
	    equal(this.radius, that.radius);
    }
    
    show (p) {
	p.disk(this.center.pt, this.radius, this.options);
    }
}


////////////////////////////////////////////////////////////
// predefined objects

const origin = new Point('O').at([0,0]);
const horizon = new Line().through(origin,0);
const vertical = new Line().through(origin,90);
const leftBorder = new Line().at([-20,0]).atAngle(90);
const rightBorder = new Line().at([20,0]).atAngle(90);
const topBorder = new Line().at([0,20]).atAngle(0);
const bottomBorder = new Line().at([0,-20]).atAngle(0);
const borders = [leftBorder,rightBorder,topBorder,bottomBorder];

const xRange = [-20,20];
const yRange = [-20,20];

class Chart {
    constructor (f)
    {
	this.paper = new Graphics(createSVG(f),
				  {'size':800,'aspectRatio':1,
				   'left-margin':10,'right-margin':10,
				   'top-margin':10,'botom-margin':10,
				   'class':'chart'})
	    .xRange(xRange)
  	    .yRange(yRange);
    }

    clear() { return this.paper.cleanPaper(); }
    
    put ()
    {
	[...arguments].flat().forEach(o => o.show(this.paper));
	return this;
    }
}
