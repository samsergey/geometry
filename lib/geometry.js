const paperSize = 50;
const xRange = [-paperSize/2,paperSize/2];
const yRange = [-paperSize/2,paperSize/2];

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
    }
}

////////////////////////////////////////////////////////////
// math

function angleV(vector) {
    // tested
    var x = vector[0], y = vector[1];
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

function lineEquation([x1, y1], [x2, y2], scale = 1) {
    // tested
    var v = [x2 - x1, y2 - y1];
    return t => point([x1, y1].vadd(v.scale(t*scale)));
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

    shift(vector) {
	// tested Point
	return this.transform(this.shiftT(vector));
    }
    
    scale(xscale, yscale = xscale) {
	// tested Point
	return this.transform(this.scaleT(xscale, yscale));
    }

    scaleAt(pnt, xscale, yscale = xscale) {
	// tested Point
	var pt = pnt.xy;
	return this.shift(pt.flip()).scale(xscale, yscale).shift(pt);
    }
    
    superpose(pnt1, pnt2) {
	// tested Point
	return this.shift(line(pnt1, pnt2).vector);
    }
    
    rotate(ang) {
	// tested Point
	return this.transform(this.rotT(deg2rad(ang)));
    }

    rotateAt(pnt, ang) {
	// tested Point
	if (pnt instanceof Point) {
	    var pt = pnt.xy;
	    return this.shift(pt.flip()).rotate(ang).shift(pt);
	}
	console.error('rotation could be done against a Point');
	return this;
    }

    reflect (ang) {
	// tested Point
	return this.transform(this.reflectT(deg2rad(ang)));
    }
    
    reflectAt(point_or_line, ang) {
	// tested Point
	var that = point_or_line;
	if (that instanceof Point) {
	    var l = new Line().through(that).atAngle(ang);
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

    option (opt, x = null) {
	if (x) {
	    this.options[opt] = x;
	    return this;
	}
	else
	    return this.options[opt];
    }

    style (c) { return this.option('class', c); }

    label (l) {
	if (l)
	    return this.option('label', l);
	else
	    return this.option('label');
    }

    get isLabeled () { return !!this.options['label']; }

    named (l) { return this.option('label', l); }

    isEqual (obj) { return 'unimplemented'; }

    isSimilar (obj) { return 'unimplemented'; }
}

////////////////////////////////////////////////////////////
// Group

class Group extends Figure {
    constructor(figures) {
	super();
	this.contents = figures;
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
	// tested
	return this.contents[(i - 1) % this.contents.length];
    }
    
    show (paper)
    {
	// tested
	this.contents.forEach(f => f.show(paper));
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
	    'pointSize' : 3, 
	    'label' : label, 
	    'labelOffset': [0, 1],
	    'point-style': style.point
	};
	this.options = Object.assign(defaults, this.options);
	this.xy = [0, 0];
    }

    isEqual (obj) {
	// tested
	return (obj instanceof Point) &&
	    equal(this.xy, obj.xy);
    }

    copy() {
	// tested
	var res = new Point();
	res.xy = this.xy.copy();
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }
    
    transform (T) {
	// tested
	this.xy = this.trans(T, this.xy);
	return this;
    }

    get x () { return this.xy[0]; }
    get y () { return this.xy[1]; }

    get isInside () {
	// tested
	return this.x > xRange[0] && this.x < xRange[1] &&
 	    this.y > yRange[0] && this.y < yRange[1];
    }
    
    at (xy) {
	// tested
	this.xy = xy;
	return this;
    }
    
    on (curve, t) {
	// tested
	if (curve instanceof Line || curve instanceof Circle) 
	    return this.at(curve.eqn(t).xy);
	else
	    console.error('on: point could be found on a Line or a Circle.');
	return this;
    }

    distance (obj) {
	// tested
	if (obj instanceof Point)
	    return seg(this, obj).length;
	if (obj instanceof Line)
	    return new Segment().through(this).heightTo(obj).length;
	if (obj instanceof Circle)
	    return abs(this.distance(obj.center)-obj.radius);
	return [];
    }

    between (point1, point2, t = 1/2) {
	// tested
	return this.on(new Line().joining(point1, point2), t);
    }

    intersection (curve1, curve2, n = 0) {
	// tests on curves
	if (!(curve1.isCurve && curve2.isCurve))
	    console.error('intersection of non vectors');
	this.xy = curve1.intersection(curve2)[n].xy;
	return this;
    }

    azimuth (point1, angle1, point2, angle2) {
	// tested
	var s = new Line().joining(point1, point2);
	var s1 = new Line().through(point1).atAngle(s.angle + angle1);
	var s2 = new Line().through(point2).atAngle(s.angle + 180 - angle2);
	if (s1.isParallelTo(s2))
	    return this.at([1/0,1/0]);
	else
	    return this.at(s1.intersection(s2)[0].xy);
    }
    
    labelOffset (offset) {
	this.option('labelOffset', offset.normalize().scale(1.2));
	return this;
    }
    
    show (paper) {
	if (Number.isFinite(this.x) && Number.isFinite(this.y)) {
	    paper.listPlot([this.xy], this.options);
	
	    if (this.isLabeled) {
		var l = this.label();
		var lp = this.xy.vadd([-0.3*l.length, -0.5]);
		label(paper, l, lp.vadd(this.option('labelOffset')));
	    }
	}
    }
}

const point = ([x, y]) => new Point().at([x, y]);

function anyPoint(n) {
    var x = () => round(Math.random()*paperSize)-paperSize/2;
    var y = () => round(Math.random()*paperSize)-paperSize/2;
    var res = range(n).map(() => new Point().at([x(),y()]));
    return new Group(res);
}

////////////////////////////////////////////////////////////
// Line

class Line extends Figure {
    constructor(label) {
	super();
	var defaults = {
	    'arrow' : false, 
	    'arrowPosition' : 1, 
	    'label' : label, 
	    'point-style': style.point,
	    'line-style': style.line
	};
	this.options = Object.assign(defaults, this.options);
	this.pivot = [0, 0];
	this.vector = [1, 0];
	this.eqn = t => point([t, 0]);
	this.resetEqn();
    }

    get isCurve () { return true; }

    copy() {
	// tested
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
	return this.joining(this.eqn(0).transform(T),
			    this.eqn(1).transform(T));
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
	return abs(cross(p.xy.vsub(this.pivot), this.unit)) < 1e-13;
    }
    
    isParallelTo (l) { return abs (l.angle - this.angle) % 180 == 0; }

    joining(p1, p2) {
	this.pivot = p1.xy;
	this.vector = p2.xy.vsub(this.pivot);
	this.resetEqn();
	return this;
    }
    
    at(xy) {
	this.pivot = xy;
	this.resetEqn();
	return this;
    }

    location (pnt) {
	return pnt.xy.vsub(this.pivot).dot(this.unit)/this.vector.norm();
    }
    
    atAngle(a) {
	var r = deg2rad(a);
	this.vector = [cos(r), sin(r)];
	this.resetEqn();
	return this;
    }
    
    intersection (obj) {
	if (obj instanceof Line)
	    return [point(intersectionV(this.pivot, this.unit, obj.pivot, obj.unit))];
	if (obj instanceof Circle)
	    return obj.intersectionL(this);
	if (obj instanceof Polygon)
	    return obj.intersection(this);
	return [];
    }
    
    through (p) { return this.at(p.xy); }
    parallelTo (l) { return this.atAngle(l.angle); }
    along (l) { return this.parallelTo(l); }
    perpendicularTo (l) { return this.atAngle(l.angle+90); }
    bisectrisse (an) { return this.atAngle((an.start + an.end)/2); }
    midPerpendicular (s) { return this.through(s.eqn(1/2)).perpendicularTo(s); }

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
    
    distance (obj) {
	if (obj instanceof Point)
	    return obj.distance(this);
	if (obj instanceof Circle)
	    return abs(this.distance(obj.center) - obj.radius);
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
	var ts = borders.map(b => this.location(this.intersection(b)[0].xy));
	var t2 = ts.filter(x => x > 0).min();
	var t1 = ts.filter(x => x < 0).max();

	p.line([this.eqn(t1).xy, this.eqn(t2).xy], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).xy.vsub(this.unit.scale(2));
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    label(p, this.option('label'), lpos.vadd(loff));
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    p.arrow([this.eqn(x - 0.1).xy, this.eqn(x).xy], this.options);
	}
    }
}

const line = (a, b) => new Line().joining(a, b);

class Ray extends Line {

    constructor(label) {
	super(label);
	var defaults = { };
	this.options = Object.assign(defaults, this.options);
    } 

    copy() {
	var res = new Ray().joining(this.eqn(0),this.eqn(1));
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }
    
    flip () {
	return this.atAngle(this.angle+180);
    }

    contains (p) {
	return super.contains(p) && this.location(p.xy) >= 0;
    }
    
    isEqual (that) {
	return (that instanceof Ray) &&
	    super.isEqual(that);
    }

    show (p) {
	var ts = borders.map(b => this.location(this.intersection(b)[0]));
	var t2 = ts.filter(x => x > 0).min();

	p.line([this.eqn(0).xy, this.eqn(t2).xy], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).xy.vsub(this.unit.scale(2));
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    label(p, this.option('label'), lpos.vadd(loff));
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    p.arrow([this.eqn(x - 0.1).xy, this.eqn(x).xy], this.options);
	}
    }
}

const ray = (a, b) => new Ray().joining(a, b);

class Segment extends Line {
    constructor(label) {
	super(label);
	var defaults = {
	    'points':true,
	    'mark' : 0, 
	    'label': label, 
	    'labelPosition' : 1/2, 
	    'labelOffset' : 1.25
	};
	this.options = Object.assign(defaults, this.options);
    }

    copy() {
	var res = new Segment().joining(this.start, this.end);
	Object.assign(res.options, this.options);
	if (this.isLabeled)
	    res.option('label', this.option('label') + "'");
	return res;
    }

    get start () { return this.eqn(0); }
    get end () { return this.eqn(1); } 
    get length () { return this.vector.norm(); } 

    contains (p) {
	return super.contains(p)
	    && this.location(p.xy) >= 0
	    && this.location(p.xy) <= 1;
    }

    extend(t1, t2)
    {
	this.pivot = this.eqn(t1).xy;
	this.vector = this.eqn(t2).xy.vsub(this.pivot);
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
	    return this.joining(point(this.pivot), c.eqn(a));
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
	p.line([this.eqn(0).xy, this.eqn(1).xy], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(this.option('labelPosition')).xy;
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    label(p, this.option('label'), lpos.vadd(loff));
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    p.arrow([this.eqn(x - 0.1).xy, this.eqn(x).xy], this.options);
	}
	if (this.option('mark') > 0)
	{
	    for (var i = 0; i < this.option('mark'); i++) {
		var m = this.eqn(1/2).xy.vadd(this.unit.scale(0.2*i));
		p.line([m.vadd(this.norm.scale(0.3)), 
			m.vadd(this.norm.scale(-0.3))], 
		       {'class':'thin'});
	    }
	}
    }

    mark (n) { return this.option('mark', n); }
    arrow () { return this.option('arrow', true); }
    points (t) { return this.option('points', t); }

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
	    'label' : 'value',
	    'line-style': style.angle,
	};
	this.options = Object.assign(defaults, this.options);
	this.start = 0;
	this.end = value;
	this.vertex = [0, 0];
    }
   
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
	var v = this.value;
	this.vertex = point(l.pivot);
	this.start = l.angle;
	this.end = this.start + v;
	return this;
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
	this.vertex = s1.eqn(0);
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
	    var p0 = this.vertex.xy;
	    var p1 = this.startRay.unit;
	    var p2 = this.endRay.unit;
	    p.line([p1, p1.vadd(p2), p2].map(p => p0.vadd(p)), this.options);
	    return;
	}

	for(var i = 0; i < this.options['strokes']; i++)
	    p.arc(this.vertex.xy, 2-i*0.3, this.end, this.start, this.options);

	if (this.isLabeled)
	{
	    var l = this.option('label')
	    if (l == 'value')
		l = abs(round(this.value))+'°';
	    var r = this.option('labelRadius');
	    var lpos = new Ray().bisectrisse(this).unit.scale(r)
		.vadd(this.vertex.xy).vadd([-0.5,-0.7]);
	    label(p,l,lpos);
	    
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
	    'points' : true,
	    'label':label,
	    'segmentLabels':[],
	    'point-style': style.point,
	    'line-style': style.polygon
	};
	this.options = Object.assign(defaults, this.options);
	this.vertices = [];
    }

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
	return new Segment().joining(v, s.eqn(1/2));
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
	    'style': style.circle
	};
	this.options = Object.assign(defaults,this.options);
	this.center = origin.copy();
	this.radius = r || 1;
	this.resetEqn();
    }

    get isCurve () { return true; }

    resetEqn() {
	var c = this.center.xy;
	var r = this.radius;
	this.eqn = t => point([c[0]+r*cos(t),c[1]+r*sin(t)]);
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
	var m = r.middle.xy.vadd(r.unit.scale(a));
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

    intersection (obj) {
	if (obj instanceof Line)
	    return this.intersectionL(obj);
	if (obj instanceof Circle)
	    return this.intersectionC(obj);
	if (obj instanceof Polygon)
	    return obj.intersection(this);
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
	    this.center = seg(p1,p2).middle;
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

    distance (obj) {
	if (obj instanceof Point || obj instanceof Line)
	    return obj.distance(this);
	else if (obj instanceof Circle)
	    return abs(this.distance(obj.center) - obj.radius - this.radius);
	else
	    console.error('object must be a point or a circle!');
	return 0;
    }

    isEqual (obj) {
	return (obj instanceof Circle) &&
	    equal(this.center, obj.center) &&
	    equal(this.radius, obj.radius);
    }
    
    show (p) {
	p.disk(this.center.xy, this.radius, this.options);
    }
}

////////////////////////////////////////////////////////////
// predefined objects

const origin = new Point('O').at([0,0]);
const horizon = new Line().through(origin,0);
const vertical = new Line().through(origin,90);
const leftBorder = new Line().at([-xRange[0],0]).atAngle(90);
const rightBorder = new Line().at([xRange[1],0]).atAngle(90);
const topBorder = new Line().at([0,yRange[0]]).atAngle(0);
const bottomBorder = new Line().at([0,-yRange[0]]).atAngle(0);
const borders = [leftBorder,rightBorder,topBorder,bottomBorder];

class Chart {
    constructor (f)
    {
	this.svg = new SVG(f, Object.assign({'size':600},style.svg))
	this.paper = new Graphics(this.svg,
				  {'size':600,'aspectRatio':1,
				   'left-margin':10,'right-margin':10,
				   'top-margin':10,'botom-margin':10,
				   'class':'chart'})
	    .xRange(xRange)
  	    .yRange(yRange);
    }

    save (name) { this.svg.save(name) }
    
    clear() { return this.paper.cleanPaper(); }
    
    put ()
    {
	[...arguments].flat().forEach(o => o.show(this.paper));
	this.paper.adjustSize();
	return this
    }
}

function label(p,l,pnt,opts)
{
    options = Object.assign({'at':pnt,'style':style.label}, opts)
    return p.label(l, options)
}
