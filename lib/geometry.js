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
    constructor (label) {
	this.options = {
	    'label' : label,
	    'labelPosition': [0, 0],
	    'labelOffset': [0, 1],
	};
    }

    copy () {
	var res = new Figure();
	Object.assign(res.options, this.options);
	return res;
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
    
    reflectAt(point_or_line) {
	// tested Point
	var that = point_or_line;
	if (that instanceof Point) {
	    return this.scaleAt(that,-1);
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

    label (l = null) {
	if (l === null )
	    return this.options['label'];
	else
	    return this.options['label'] = l;
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
	return this.copy();
    }

    element (i) {
	// tested
	return this.contents[(i - 1) % this.contents.length];
    }
    
    show (paper)
    {
	// tested
	this.contents.forEach(f => f.show(paper));
    }
}

////////////////////////////////////////////////////////////
// Point

class Point extends Figure {
    constructor (label)
    {
	super(label);
	var defaults = {
	    'pointSize' : 3, 
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

    copy () {
	// tested
	var res = new Point();
	Object.assign(res.options, this.options);
	res.xy = this.xy.copy();
	return res;
    }
    
    transform (T) {
	// tested
	var res = this.copy();
	res.xy = this.trans(T, res.xy);
	return res;
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
	var res = this.copy();
	res.xy = xy;
	return res;
    }
    
    on (curve, t) {
	// tested
	if (curve instanceof Line || curve instanceof Circle) 
	    return this.at(curve.eqn(t).xy);
	else
	    console.error('on: point could be found on a Line or a Circle.');
	return this;
    }

    between (point1, point2, t = 1/2) {
	// tested
	return this.on(new Line().joining(point1, point2), t);
    }

    intersection (curve1, curve2, n = 0) {
	// tests on curves
	if (!(curve1.isCurve && curve2.isCurve))
	    console.error('intersection of non vectors');
	return this.at(curve1.intersections(curve2)[n].xy);
    }

    azimuth (point1, angle1, point2, angle2) {
	// tested
	var s = new Line().joining(point1, point2);
	var s1 = new Line().through(point1).atAngle(s.angle + angle1);
	var s2 = new Line().through(point2).atAngle(s.angle + 180 - angle2);
	if (s1.isParallelTo(s2))
	    return this.at([1/0,1/0]);
	else
	    return this.at(s1.intersections(s2)[0].xy);
    }
    
    distance (obj) {
	// tested
	if (obj instanceof Point)
	    return seg(this, obj).length;
	if (obj instanceof Line)
	    return new Segment().through(this).heightTo(obj).length;
	if (obj instanceof Circle)
	    return abs(this.distance(obj.center)-obj.R);
	return NaN;
    }
    
    labelOffset (offset) {
	return this.option('labelOffset', offset.normalize().scale(1.2));;
    }
    
    show (paper) {
	if (Number.isFinite(this.x) && Number.isFinite(this.y)) {
	    paper.listPlot([this.xy], this.options);
	
	    if (this.isLabeled) {
		var l = this.label();
		var lp = this.xy.vadd([-0.3*l.length, -0.5]);
		mkLabel(paper, l, lp.vadd(this.option('labelOffset')));
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
	    'points': false,
	    'arrow' : false, 
	    'arrowPosition' : 1, 
	    'point-style': style.point,
	    'line-style': style.line
	};
	this.options = Object.assign(defaults, this.options);
	this.pivot = [0, 0];
	this.vector = [1, 0];
	this.eqn = t => point([t, 0]);
	this.resetEqn();
    }

    static vect(point1, point2) {
	return point2.xy.vsub(point1.xy);
    }
    
    get isCurve () { return true; }

    copy () {
	// tested
	var res = new Line();
	Object.assign(res.options, this.options);
	res.pivot = this.pivot.copy();
	res.vector = this.vector.copy();
	res.resetEqn();
	return res;
    }
    
    transform (T) {
	// tested
	var res = this.copy();
	res.pivot = this.trans(T, this.pivot);
	var x = this.trans(T, this.pivot.vadd(this.vector));
	res.vector = x.vsub(res.pivot);
	res.resetEqn();
	return res;
    }
   
    get unit () { return this.vector.normalize(); }
    get norm () { return [-this.unit[1], this.unit[0]]; } 
    get middle () { return this.eqn(1/2); } 
    get angle () { return angleV(this.unit); }

    resetEqn() {
	this.eqn = lineEquation(this.pivot, this.pivot.vadd(this.vector));
    }

    location (pnt) {
	// tested
	return pnt.xy.vsub(this.pivot).dot(this.unit)/this.vector.norm();
    }

    isContaining (point) {
	// tested
	return abs(cross(point.xy.vsub(this.pivot), this.unit)) < 1e-13;
    }
    
    isParallelTo (line) {
	// tested
	return equalMod(180, line.angle,  this.angle);
    }

    isPerpendicularTo (line) {
	// tested
	return equalMod(180, line.angle - this.angle, 90);
    }

    isTangentTo (circle) {
	return equal(circle.center.distance(this), circle.R);
    }
    
    isEqual (that) {
	// tested
	return (that instanceof Line) &&
	    equal(this.pivot, that.pivot) &&
	    equal(this.vector, that.vector); 
    }

    isSimilar (obj) {
	// tested
	return (obj instanceof Line) &&
	    this.isContaining(obj.eqn(0)) &&
	    this.isContaining(obj.eqn(1));
    }

    joining(point1, point2) {
	// tested
	var res = this.copy();
	res.pivot = point1.xy;
	res.vector = point2.xy.vsub(res.pivot);
	res.resetEqn();
	return res;
    }
    
    at(xy) {
	// tested
	var res = this.copy();
	res.pivot = xy;
	res.resetEqn();
	return res;
    }
  
    atAngle(angle_degrees) {
	// tested
	var res = this.copy();
	var r = deg2rad(angle_degrees);
	res.vector = [cos(r), sin(r)];
	res.resetEqn();
	return res;
    }
        
    through (point) {
 	// tested
	return this.at(point.xy);
    }
    
    parallelTo (line) {
	// tested
	return this.atAngle(line.angle);
    }
    
    along (line) {
	// tested
	return this.parallelTo(line);
    }
    
    perpendicularTo (line) {
	// tested
	return this.atAngle(line.angle+90);
    }
    
    bisectrisse (angle) {
	// tested
	return this.atAngle((angle.start + angle.end)/2);
    }
    
    midPerpendicular (segment) {
	// tested
	return this.through(segment.eqn(1/2)).perpendicularTo(segment);
    }

    tangentTo(circle, direction = 1) {
	// tested
	if (this.eqn(0).distance(circle.center) < circle.R) {
	    console.error("The pivot point of the tangent line is inside the circle!");
	    return this;
	}
	var d = seg(point(this.pivot), circle.center);
	var a = direction * rad2deg(Math.asin(circle.R/d.length));
	return this.atAngle(d.angle+a);
    }
    
    distance (obj) {
	// tested
	if (obj instanceof Point)
	    return obj.distance(this);
	if (obj instanceof Circle)
	    return abs(this.distance(obj.center) - obj.R);
	else
	    console.error('object must be a point or a circle!');
	return 0;
    }

    intersections (obj) {
	// tested
	if (obj instanceof Line)
	    return [point(intersectionV(this.pivot, this.unit, obj.pivot, obj.unit))];
	if (obj instanceof Circle)
	    return obj.intersectionL(this);
	if (obj instanceof Polygon)
	    return obj.intersections(this);
	return [];
    }

    isIntersecting (curve) {
	return this.intersections(curve)
	    .filter(p => this.isContaining(p)
		    && curve.isContaining(p));
    }
    
    show (paper) {
	var ts = borders.map(b => this.location(b.intersections(this)[0]));
	var t2 = ts.filter(x => x > 0).min();
	var t1 = ts.filter(x => x < 0).max();

	paper.line([this.eqn(t1).xy, this.eqn(t2).xy], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).xy.vsub(this.unit.scale(2));
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    mkLabel(paper, this.option('label'), lpos.vadd(loff));
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    paper.arrow([this.eqn(x - 0.1).xy, this.eqn(x).xy], this.options);
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
   
    copy () {
	// tested
	var res = new Ray();
	Object.assign(res.options, this.options);
	res.pivot = this.pivot.copy();
	res.vector = this.vector.copy();
	res.resetEqn();
	return res;
    }

    get start () { return this.eqn(0); }
    
    isContaining (point) {
	// tested
	return super.isContaining(point)
	    && this.location(point) >= 0;
    }
    
    isEqual (obj) {
	// tested
	return (obj instanceof Ray)
	    && super.isEqual(obj);
    }

    isSimilar (obj) {
	// tested
	return (obj instanceof Ray)
	    && this.start.isEqual(obj.start)
	    && equal(this.unit,obj.unit);
    }

    flip () {
	// tested
	return this.reflectAt(this.start);
    }
    
    show (paper) {
	var ts = borders.map(b => this.location(this.intersections(b)[0]));
	var t2 = ts.filter(x => x > 0).min();

	paper.line([this.eqn(0).xy, this.eqn(t2).xy], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(t2).xy.vsub(this.unit.scale(2));
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    mkLabel(paper, this.option('label'), lpos.vadd(loff));
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    paper.arrow([this.eqn(x - 0.1).xy, this.eqn(x).xy], this.options);
	}
    }
}

const ray = (a, b) => new Ray().joining(a, b);

class Segment extends Line {
    constructor(label) {
	super(label);
	var defaults = {
	    'mark' : 0, 
	    'labelPosition' : 1/2, 
	    'labelOffset' : 1.25
	};
	this.options = Object.assign(defaults, this.options);
    }

    copy () {
	// tested
	var res = new Segment();
	Object.assign(res.options, this.options);
	res.pivot = this.pivot.copy();
	res.vector = this.vector.copy();
	res.resetEqn();
	return res;
    }

    get start () { return this.eqn(0); }
    get end () { return this.eqn(1); } 
    get length () { return this.vector.norm(); } 

    isEqual (obj) {
	return (obj instanceof Segment) &&
	    super.isEqual(obj);
    }

    isSimilar (segment) {
	//tested
	return (segment instanceof Segment)
	    && equal(this.length, segment.length);
    }

    isContaining (point) {
	//tested
	return super.isContaining(point)
	    && this.location(point) >= 0
	    && this.location(point) <= 1;
    }
    
    extend (t1, t2 = null) {
	//tested
	if (!t2)
	    return this.joining(this.eqn(0), this.eqn(t1));
	else
	    return this.joining(this.eqn(t1), this.eqn(t2));
    }

    extendToLength (number) {
	// tested
	var p = this.pivot.vadd(this.unit.scale(number));
	return this.joining(this.start, point(p));
    }

    extendToLine (line) {
	// tested
	if (this.isParallelTo(line))
	    return new Ray().through(this.start).parallelTo(this);
	else
	    return this.joining(this.start, line.intersections(this)[0]);
    }

    heightTo (line) {
	// tested
	return this.perpendicularTo(line).extendToLine(line);
    }

    tangentTo(circle, direction = 1) {
	// tested
	var d = this.start.distance(circle.center);
	var t = new Ray().through(this.start).tangentTo(circle, direction);
	if (equal(d, circle.R)) {
	    return this.parallelTo(t);
	} else if (d < circle.R) {
	    console.log();
	    return this;
	} else {
	    var p = this.parallelTo(t).extendToLength(sqrt(d**2-circle.R**2)).end;
	    var a = circle.location(p);
	    return this.joining(this.start, circle.eqn(a));
	}
    }
    
    intersections (curve) {
	// tested
	return super.intersections(curve);//.filter(p => this.isContaining(p));
    }

    show (paper)
    {
	paper.line([this.eqn(0).xy, this.eqn(1).xy], this.options);
	
	if (this.isLabeled)
	{
	    var lp = this.eqn(this.option('labelPosition')).xy;
	    var lpos = lp.vadd([-0.5, -0.5]);
	    var loff = this.norm.scale(this.option('labelOffset'));
	    mkLabel(paper, this.label(), lpos.vadd(loff));
	}
	if (this.option('arrow'))
	{
	    var x = this.option('arrowPosition');
	    paper.arrow([this.eqn(x - 0.1).xy, this.eqn(x).xy], this.options);
	}
	if (this.option('mark') > 0)
	{
	    for (var i = 0; i < this.option('mark'); i++) {
		var m = this.eqn(1/2).xy.vadd(this.unit.scale(0.2*i));
		paper.line([m.vadd(this.norm.scale(0.3)), 
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
	super('value');
	var defaults = {
	    'strokes' : 1, 
	    'labelRadius' : 3, 
	    'line-style': style.angle,
	};
	this.options = Object.assign(defaults, this.options);
	this.start = 0;
	this.end = value;
	this.vertex = [0, 0];
    }
   
    copy () {
	//tested
	var res = new Angle();
	Object.assign(res.options, this.options);
	res.start = this.start;
	res.end = this.end;
	res.vertex = this.vertex.copy();
	return res;
    }

    isEqual (obj) {
	//tested
	return (obj instanceof Angle)
	    && this.start.isEqual(obj.start)
	    && this.end.isEqual(obj.end)
	    && this.pivot.isEqual(obj.pivot);
    }

    isSimilar (obj) {
	//tested
	return (obj instanceof Angle)
	    && equal(this.value, obj.value);
    }

    on (line) {
	//tested
	var res = this.copy();
	res.vertex = point(line.pivot);
	res.start = line.angle;
	res.end = res.start + this.value;
	return res;
    }

    at (point) {
	//tested
	var res = this.copy();
	res.vertex = point;
	return res;
    }
    
    transform (T) {
	//tested
	var res = this.copy();
	res.vertex = this.vertex.transform(T);
	res.start = this.startRay.transform(T).angle();
	res.end = this.endRay.transform(T).angle();
	return res;
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
    
    
    vertical () {
	this.start = (this.start + 180) % 360;
	this.end = (this.end + 180) % 360;
	return this.copy();
    }

    adjacent () {
	[this.start, this.end] = [this.end, (this.start + 180) % 360];
	return this.copy();
    }

    complement () {
	[this.start, this.end] = [this.end, this.start];
	return this.copy();
    }
    
    within (point1, point2, point3) {
	this.start = ray(point2, point1).angle;
	this.end = ray(point2, point3).angle;
	this.vertex = point2;
	return this.copy();
    }
    
    between (line1, line2) {
	this.start = line1.angle;
	this.end = line2.angle;
	this.vertex = line1.eqn(0);
	return this.copy();
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
	    var l = this.option('label');
	    if (l == 'value')
		l = abs(round(this.value))+'°';
	    var r = this.option('labelRadius');
	    var lpos = new Ray().bisectrisse(this).unit.scale(r)
		.vadd(this.vertex.xy).vadd([-0.5,-0.7]);
	    mkLabel(p,l,lpos);
	    
	}
    }

    strokes (n) { return this.option('strokes', n); }
}

const angle = (a,b,c) => new Angle().within(a,b,c);

////////////////////////////////////////////////////////////
// Polygon

class Polygon extends Figure {
    constructor(label) {
	super(label);
	var defaults = {
	    'labelPosition' : 'automatic',
	    'points' : true,
	    'segmentLabels':[],
	    'point-style': style.point,
	    'line-style': style.polygon
	};
	this.options = Object.assign(defaults, this.options);
	this.vertices = [];
    }

    get isCurve () { return true; }

    copy () {
	var res = new Polygon();
	res.vertices = this.vertices.map(v => v.copy());
	Object.assign(res.options, this.options);
	return res;
    }
    
    transform (T) {
	this.vertices = this.vertices.map(v => v.transform(T));
	return this.copy();
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

    isContaining (p) { return this.segments.some(s => s.isContaining(p)); }
    
    through(ps) {
	this.vertices = ps;
	return this;
    }

    intersections (that) { return this.segments.mapappend(s => s.intersections(that)); }

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
	this.R = r || 1;
	this.resetEqn();
    }

    get isCurve () { return true; }

    get radius () {
	return seg(this.center, this.eqn(0));
    }

    resetEqn() {
	var c = this.center;
	this.eqn = t => point([c.x+this.R*cos(t),c.y+this.R*sin(t)]);
    }

    location (p) {
	return deg2rad(line(this.center, p).angle);
    }
    
    copy () {
	var res = new Circle(this.R);
	Object.assign(res.options, this.options);
	res.R = this.R;
	res.center = this.center.copy();
	res.resetEqn();
	return res;
    }
    
    transform (T) {
	var r = this.radius.transform(T);
	return new Circle(r.length).at(r.start);
    }

    at (point) {
	var res = this.copy();
	res.center = point;
	res.resetEqn();
	return res;
    }

    isContaining (p) {
	return abs(p.distance(this.center) - this.R) < 1e-14;
    }
    
    intersectionC (that) {
	var r0 = this.R, r1 = that.R;
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

    intersectionL (line) {
	var h = new Segment().through(this.center).heightTo(line);
	if (equal(h.length, this.R))
	    return [h.end];
	if (h.length > this.R)
	    return [];
	var x = sqrt(this.R**2 - h.length**2);
	var s = new Segment().through(h.end).along(line).extend(-x,x);
	return [s.start, s.end];
    }

    intersections (obj) {
	if (obj instanceof Line)
	    return this.intersectionL(obj);
	if (obj instanceof Circle)
	    return this.intersectionC(obj);
	if (obj instanceof Polygon)
	    return obj.intersections(this);
	return [];
    }
    
    through1 (point) {
	this.R = seg(this.center, point).length;
	this.resetEqn();
	return this.copy();
    }

    through2 (p1, p2) {
	var d = p1.distance(p2);
	if (d > this.R*2) {
	    this.R = d/2;
	    this.center = seg(p1,p2).middle;
	} else {
	    var c1 = new Circle(this.R).at(p1);
	    var c2 = new Circle(this.R).at(p2);
	    this.center = c1.intersectionC(c2)[0];
	}
	this.resetEqn();
	return this.copy();
    }

    through3 (p1,p2,p3) {
	var l1 = new Line().midPerpendicular(seg(p1,p2));
	var l2 = new Line().midPerpendicular(seg(p2,p3));
	if (l1.isParallelTo(l2))
	    console.error("Can't build a circle, three points are aligned.");
	this.center = new Point().intersections(l1,l2)[0];
	this.R = this.center.distance(p1);
	this.resetEqn();
	return this.copy();
    }

    through (point1, point2, point3) {
	if (!point2  && !point3)
	    return this.through1(point1);
	else if (!point3)
	    return this.through2(point1, point2) ;
	else
	    return this.through3(point1, point2, point3);
    }

    distance (obj) {
	if (obj instanceof Point || obj instanceof Line)
	    return obj.distance(this);
	else if (obj instanceof Circle)
	    return abs(this.distance(obj.center) - obj.R - this.R);
	else
	    console.error('object must be a point or a circle!');
	return 0;
    }

    isEqual (obj) {
	return (obj instanceof Circle) &&
	    equal(this.center, obj.center) &&
	    equal(this.R, obj.R);
    }
    
    show (paper) {
	paper.disk(this.center.xy, this.R, this.options);
    }
}

////////////////////////////////////////////////////////////
// predefined objects

const origin = new Point('O').at([0,0]);
const Ox = new Line().through(origin).atAngle(0);
const Oy = new Line().through(origin).atAngle(90);
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
//	this.paper.adjustSize();
	return this
    }
}

function mkLabel(p,l,pnt,opts)
{
    options = Object.assign({'at':pnt,'style':style.label}, opts)
    return p.label(l, options)
}
