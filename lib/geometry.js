/** A pair of numbers, representing a vector.
 * @typedef {Pair<Number>} XY
 * @example
 * [2, 3]
 */

/** A matrix as a list of list
 * @typedef {Array<Array<Number>>} Matrix
 * @example
 * [[2, 0, 0],[0, g3, 0],[0, 0, 1]
 */

/** An object representing CSS style
 * @typedef {Object} Style
 * @example
 * {'stroke':'red', 'stroke-opacity':0.5}
 */

/** Representation of a boundeng box
 * @typedef {Pair<XY>} Box
 * @example
 * new Circle(2).box 
 * > [[-2,-2],[2,2]]
 */

/** Representation of a postion within a bounding box. sign ∈ {-1, 0, 1}
 * @typedef {sign} AlignmentIndex 
 * @example
 * new Circle(2).beside(new Square(), -1) // aligned at bottomline 
 * new Circle(2).beside(new Square(), 0) // aligned at centerline
 * new Circle(2).beside(new Square(), 1) // aligned at topline
 */

/** An angle in degrees.
 * @typedef {Number} angle
 */

/** @namespace Global */

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

    mark : {
	'stroke':'wheat',
	'stroke-width':1
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
	'fill':'white',
    },
    
    text : {
	'font-family' : "'CMU Sans Serif', sans-serif",
	'font-style' : 'italic',
	'font-size' : '18px',
	'fill':'white',
    }
}

////////////////////////////////////////////////////////////
// math

function isXY (obj) {
    return (obj instanceof Array)
	&& obj.length == 2
	&& obj.every(Number.isFinite)
}

function isPosition (obj) {
    return obj instanceof Point || isXY(obj)
}

function angleV(vector) {
    // tested
    const x = vector[0], y = vector[1]
    if (x == 0 && y == 0) return NaN
    if (x == 1 && y == 0) return 0
    if (x == 0 && y == 1) return 90
    if (x == -1 && y == 0) return 180
    if (x == 0 && y == -1) return 270
    const a = rad2deg(Math.atan(y/x))
    if (x > 0 && y > 0) return a
    if (x < 0 && y > 0) return 180 + a
    if (x < 0 && y < 0) return 180 + a
    if (x > 0 && y < 0) return 360 + a
    return mod(a,360)
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

/** Returns a vector between two points
 * @param {Point|XY} pos1
 * @param {Point|XY} pos2
 * @return {XY}
 */
function vector (pos1, pos2) {
    return Point.iso(pos2).xy.vsub(Point.iso(pos1).xy)
}

/** Returns a vector with a given angle
 * @param {Angle} an
 * @param {Number} r
 * @return {XY}
 */
function polarVector (an, r = 1) {
    return [r*cos(deg2rad(an)), r*sin(deg2rad(an))]
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
/** Abstract class representing a geometric figure.
 * Provides basic linear transformations.
 */
class Figure {
    constructor (label) {
	this.options = {
	    'label' : label
	}
    }

    /** Makes functional copy of an object.
     * @return {Figure} a copy of a caller
     */
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

    /** Object fields equivalence.
     * @return {bool}
     */
    isEqual (obj) { return this == obj }

    /** Geometric equivalence.
     * @return {bool}
     */
    isSimilar (obj) { return this.isEqual(obj) }
    
    /** General linear transformation. Returns a copy of the figure.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Figure} a copy of a caller
     */
    transform (T) {
	console.error("transform is unimplemented")
    }

    /** The refference point of a figure. Used for superposition of figures.
     * @type {Point}
     */
    get refPoint () { return point([0,0]) }
    
    /** Transformation. Locates refference point of a figure at given position.
     * @return {Figure} a copy of a caller
     * @param {Point | XY} pos
     */
    at (pos) {
	return this.superpose(this.refPoint, Point.iso(pos).point)
    }

    /** Transformation. Parallel translation along a given vector.
     * @return {Figure} a copy of a caller
     * @param {XY} vector
     */
    translate(vector) {
	return this.transform(translateT(vector))
    }
    
    /** Transformation. Scaling along the axis.
     * @return {Figure} a copy of a caller
     * @param {Number} xscale
     * @param {Number} [yscale = xscale]
     */
    scale(xscale, yscale = xscale) {
	return this.transform(scaleT(xscale, yscale))
    }

    /** Transformation. Scaling along the axis at a given point.
     * @return {Figure} a copy of a caller
     * @param {Point|XY} pos
     * @param {Number} xscale
     * @param {Number} [yscale = xscale]
     */
    scaleAt(pos, xscale, yscale = xscale) {
	const pt = Point.iso(pos).xy
	return this.translate(pt.flip()).scale(xscale, yscale).translate(pt)
    }

    /** Transformation. Stretching along the given line, ray or segment.
     * @return {Figure} a copy of a caller
     * @param {Line} line
     * @param {Number} scale
     */
    stretch(line, scale) {
	return this.rotate(-line.angle(0)).scale(1,scale).rotate(line.angle(0))
    }

    /** Transformation. Stretching along the given line, ray or segment at a given point.
     * @return {Figure} a copy of a caller
     * @param {Point | XY} pos
     * @param {Line|Ray|Segment} line
     * @param {Number} scale
     */
    stretchAt(pos, line, scale) {
	const pt = Point.iso(pos).xy
	return this.translate(pt.flip()).stretch(line, scale).translate(pt)
    }

    /** Transformation. Translation resulting in the superposition of two given points.
     * @return {Figure} a copy of a caller
     * @param {Point | XY} pnt1 moving point
     * @param {Point | XY} pnt2 fixed point
     */
    superpose(pnt1, pnt2) {
	return this.translate(vector(pnt1, pnt2))
    }

    /** Transformation. Translation and rotation resulting in the superposition of two given lines, rays or segments.
     * @return {Figure} a copy of a caller
     * @param {Line|Ray|Segment} line1 moving line
     * @param {Line|Ray|Segment} line2 fixed line
     */
    align(line1, line2) {
	// tested Point
	return this.rotateAt(line1.point(0), line2.angle(0) - line1.angle(0))
    }

    /** Transformation. Rotation around the origin.
     * @return {Figure} a copy of a caller
     * @param {angle} ang angle counting CCW
     */
    rotate(ang) {
	return this.transform(rotT(deg2rad(ang)))
    }

    /** Transformation. Rotation around the given point.
     * @return {Figure} a copy of a caller
     * @param {Point|XY} pos 
     * @param {angle} ang angle counting CCW
     */
    rotateAt(pos, ang) {
	const pt = Point.iso(pos).xy
	return this.translate(pt.flip()).rotate(ang).translate(pt)
    }

    /** Transformation. Reflection against the axis passing through the origin at given angle.
     * @return {Figure} a copy of a caller
     * @param {angle} ang angle counting CCW
     */
    reflect (ang) {
	return this.transform(reflectT(deg2rad(ang)))
    }
    
    /** Transformation. Reflection against the given point or line. If a point is given then central reflection is performed. If a line is given then axial reflection is done.
     * @return {Figure} a copy of a caller
     * @param {Point|Line|Ray|Segment} point_or_line
     */
    reflectAt(point_or_line) {
	const that = point_or_line

	console.assert(isPosition(that) || that instanceof Line,
		       'reflection could be done against Point or Line')

	if (isPosition(that)) {
	    return this.scaleAt(that,-1)
	}
	if (that instanceof Line) {
	    const b = that.angle(0),
		  pt = that.pivot
	    return this.translate(pt.flip()).reflect(b).translate(pt)
	}	
	return this
    }

    /** Predicate. Returns true if figure is trivial in some sence. */
    get isTrivial () { return false }

    /** Predicate. Returns false if figure is trivial in some sence. */
    get isNontrivial () { return !this.isTrivial }

    /** Selector. Returns the type of an object. */
    static type (obj) {
	return (obj instanceof Point ? 'Point' :
		obj instanceof Segment ? 'Segment' :
		obj instanceof Ray ? 'Ray' :
		obj instanceof Line ? 'Line' :
		obj instanceof Triangle ? 'Triangle' :
		obj instanceof Square ? 'Square' :
		obj instanceof Rectangle ? 'Rectangle' :
		obj instanceof Quadrilateral ? 'Quadrilateral' :
		obj instanceof Circle ? 'Circle' :
		obj instanceof Curve ? 'Curve' :
	    	obj instanceof Angle ? 'Angle' :
		obj instanceof Figure ? 'Figure' :
		isPosition(obj) ? 'XY' :
		undefined)
    }
    
    /** Unpure. Sets or returns the option of an object.
     * @return {Any} option value or a Figure with new option setting.
     * @param {String} option
     * @param {Any} [value = null]
     */
    option (opt, x = null) {
	if (x === null) {
	    return this.options[opt]
	} else {
	    this.options[opt] = x
	    return this
	}
    }
    
    /** Unpure. Sets or returns the label of an object.
     * @return {String|Figure} label or a Figure with new leabel.
     * @param {String} [value = null]
     */
    label (l = null) {
	if (l === null )
	    return this.option('label')
	else
	    return this.option('label', l)
    }

    /** Predicate. Returns <tt>true</tt> if Figure has a label.
     * @type {bool}
     */
    get isLabeled () { return !!this.options['label'] }

    /** Sets a style of lines for a figure.
     * @return {Figure} a copy of a caller
     * @param {Style} style
     */
    lineStyle (style) {
	const res = this.copy(),
	      st = Object.assign({}, res.options['line-style']),
	      ls = Object.assign(st,style)
	return res.option('line-style', ls)
    }

    /** Sets a style of points for a figure.
     * @return {Figure} a copy of a caller
     * @param {Style} style
     */
    pointStyle (attr, value) {
	const res = this.copy(),
	      st = Object.assign({}, res.options['point-style']),
	      s = {}
	s[attr] = value
	const ls = Object.assign(st,s)
	return res.option('point-style', ls)
    }

    /** Sets a dashed line
     * @return {Figure} a copy of a caller
     */
    dashed () {	return this.lineStyle({'stroke-dasharray':'5,5'}) }

    /** Sets a dotted line 
     * @return {Figure} a copy of a caller
     */
    dotted () {	return this.lineStyle({'stroke-dasharray':'2,4'}) }

    /** Sets thickness of lines
     * @return {Figure} a copy of a caller
     * @param {Number} [thickness = 1]
     */
    thin (th = 1) { return this.lineStyle({'stroke-width':th}) }

    /** Sets color of lines
     * @return {Figure} a copy of a caller
     * @param {String} color
     */
    color (c) {	return this.lineStyle({'stroke':c}) }

    /** Shows the figure.
     * @param {Paper} paper the SVG viewport, created by <tt>Graphics</tt>
     */
    show (paper) {}
}

///////////////////////////////////////////////////////////
// Boxed
/** Abstract class representing bounding box information.
 * @extends Figure
 */
class BoxedFigure extends Figure {
    constructor (label) {
	super(label)
    }
    
    /** Returns a bounding box of a figure as a pair of the lower-left corner and the upper-right corner
     * @type {Box}
     */
    get box () { return Box.mempty }
    
    /** Returns a width of a figure  
     * @type {Number}
     */
    get boxWidth () {
	return 	this.boxCorner(-1,-1).distance(this.boxCorner(1,-1))
    }

    /** Returns a height of a figure
     * @type {Number}
     */
    get boxHeight () {
	return 	this.boxCorner(-1,-1).distance(this.boxCorner(-1,1))
    }
    
    /** Returns a locus within a bounding box. Location is given by an index.
     * @return {Point}
     * @param {Pair<Number>} locus in form [dx, dy] where dx,dy ∈ [-1, 0, 1].
     * @example
     * let c = new Circle(2)
     * > c.boxCorner([-1,-1]).xy
     * [-2, 2]
     * > c.boxCorner([-1,0]).xy
     * [-2, 0]
     * > c.boxCorner([-1,1]).xy
     * [-2, 2]
     * > c.boxCorner([0,0]).xy
     * [0, 0]
     */
    boxCorner ([dx, dy]) {
	let xmin,ymin,xmax,ymax;
	[[xmin,ymin],[xmax,ymax]] = this.box
	const xs = [xmin, (xmin+xmax)/2, xmax],
	      ys = [ymin, (ymin+ymax)/2, ymax]
	return point([xs[1+Math.sign(dx)], ys[1+Math.sign(dy)]])
    }  
    
    /** Aligns two figures by superposing their boxCorners.
     * @return {Group}
     * @param {Pair<Number>} bc1 caller's boxCorner
     * @param {BoxedFigure} obj object to be moved and aligned
     * @param {Pair<Number>} bc2 moving object's boxCorner
     * @param {Pair<Number>} [spacing = [0,0]] a pair of numbers giving horizontal 
     * and vertical spacing between figures
     */
    boxAlign (bc1, obj, bc2, spacing = [0,0]) {
	return new Group([
	    this,
	    obj.superpose(obj.boxCorner(bc1), this.boxCorner(bc2))
		.translate(spacing)
	])
    }

    /** Aligns two figures horizontally
     * @return {Group}
     * @param {BoxedFigure} obj object to be moved and aligned
     * @param {AlignmentIndex} alignment -1 — bottomline, 0 — centerline, 1 — topline
     * @param {Number} [spacing = 0] horizontal spacing between figures
     */
    beside(obj, alignment = 0, space = 0) {
	return this.boxAlign([-1, alignment], obj, [1, alignment], [space, 0]) 
    }

    /** Aligns two figures vertically.
     * @return {Group}
     * @param {BoxedFigure} obj object to be moved and aligned
     * @param {AlignmentIndex} alignment -1 — leftside, 0 — center, 1 — rightside
     * @param {Number} [spacing = 0] vertical spacing between figures
     */
    below(obj, alignment = 0, space = 0) {
	return this.boxAlign([alignment, -1], obj, [alignment, 1], [0, space]) 
    }

    /** Aligns two figures vertically.
     * @return {Group}
     * @param {BoxedFigure} obj object to be moved and aligned
     * @param {AlignmentIndex} alignment -1 — leftside, 0 — center, 1 — rightside
     * @param {Number} [spacing = 0] vertical spacing between figures
     */
    above(obj, alignment = 0, space = 0) {
	return this.boxAlign([alignment, 1], obj, [alignment, -1], [0, -space]) 
    }

}


///////////////////////////////////////////////////////////
// Group
/** Representing a group of figures
 * @extends BoxedFigure
 */
class Group extends BoxedFigure {
    /** Creates a group of figures treated as one object. All transformations and modifications are applyed to all elements of the group. 
     * @constructor 
     * @param {Array<Figure>} figures the list of figures
     */
    constructor(figures) {
	super()
	/** The collection of figures in a group
	 * @type {Array<Figure>} */
	this.contents = figures
    }

    //------------------------------------------------------------
    // Figure implementation

    /** Makes functional copy of all objects in a group.
     * @return {Group} a copy of a caller
     */
    copy () {
	const res = new Group()
	res.contents = this.contents.map(f => f.copy())
	return res
    }

    /** The refference point of a group (left-bottom corner of a bounding box). Used for superposition of figures.
     * @type {Point}
     */
    get refPoint () { return this.boxCorner([0,0]) }

    /** General linear transformation. Returns a copy of the group.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Figure} a copy of a caller
     */
    transform (T) {
	const res = this.copy()
	res.contents = res.contents.map(f => f.transform(T))
	return res
    }

    //------------------------------------------------------------
    // Group methods

    /** Group element selector.
     * @return {Figure}
     * @param {Number} i the index, starting from 1
     */
    element (i_or_label) {
	// tested
	if (Number.isFinite(i_or_label))
	    return this.contents[(i_or_label - 1) % this.contents.length]
	else
	    return this.contents.filter(f => f.label() == i_or_label)
    }

    /** General group modifyer. Used to implement methods [<b>mapContents</b>]{@link Group#mapContents}
	and [<b>filterContents</b>]{@link Group#filterContents}.
     * @return {Figure}
     * @param {Fun<Array<Figure>, Array<Figure>>} fn the function applyed to group contents
     */
    withContents (fn) {
	let res = this.copy()
	res.contents = fn(res.contents)
	return res
    }

    /** Group modifyer.
     * @return {Figure}
     * @param {Fun<Figure, Figure>} fn the function applyed to each group element
     * @example
     * new Group().randomPoints(10)
     *            .mapContents(p => new Circle().at(p))
     */
    mapContents (fn) { this.withContents(c => c.map(fn)) }

    /** Group modifyer.
     * @return {Figure}
     * @param {Fun<Figure, bool>} fn the predicate applyed to each group element
     * @example
     * let c = new Circle(2).at([1,1])
     * new Group().randomPoints(10)
     *            .filterContents(p => c.isEnclosing(p))
     */
    filterContents (fn) { this.withContents(c => c.filter(fn)) }

    /** Group modifyer. Recursively flattens nested groups to one group.
     * @return {Group}
     * @example
     * let c = new Circle(2).at([1,1])
     * c.above(c.beside(c)).flat()
     */
    flat() {
	let res = this.copy()
	res.contents = this.contents.mapappend(
	    f => f instanceof Group ? f.flat().contents : [f])
	return res	
    }
    
    /** Creates a row of figures, locating them in a list order rightwise to the first one.
     * @static
     * @param {Array<Figure>} figs a list of figures     
     * @param {AlignmentIndex} [alignment = 0] -1 — bottomline, 0 — center, 1 — toplineside
     * @param {Number} [spacing = 0] the spacing between figures
     * @return {Group}
     * @example
     * row(range(1,5).map(r => new Circle(r)))
     */
    static row(figures, alignment = 0, space = 0) {
	return figures
	    .foldr((el,res) => el.beside(res, alignment, space))
	    .flat()
    }

    /** Creates a column of figures, locating them in a list order below the first one.
     * @static
     * @param {Array<Figure>} figs a list of figures     
     * @param {AlignmentIndex} [alignment = 0] -1 — leftside, 0 — center, 1 — rightside
     * @param {Number} [spacing = 0] the spacing between figures
     * @return {Group}
     * @example
     * column(range(1,5).map(r => new Circle(r)))
     */
    static column(figures, alignment = 0, space = 0) {
	return figures
	    .foldr((el,res) => el.above(res, alignment, space))
	    .flat()
    }

    /** Returns a group of points randomly distributed in a unit circle.
     * @type {Group}
     * @param {Number} n the number of points
     * @example
     * new Group().randomPoints(20)
     *            .scale(4,7)
     *            .rotate(45) // points distributed in an ellipse
     */
    randomPoints(n) {
	let r = () => sqrt(Math.random()),
	    a = () => 2*pi*Math.random(),
	    res = range(n).map(() => ((a,r) => point([cos(a), sin(a)].scale(r)))(a(),r()))
	return new Group(res)
    }

    /** Returns a bounding box of a group as folding of all element boxes.
     * @type {Box}
     */
    get box () { return this.contents.foldMap(f => f.box, Box) }
    
    /** Shows all group contents.
     * @param {Paper} paper the SVG viewport, created by <tt>Graphics</tt>
     */
    show (paper)
    {
	// tested
	for(let f of this.contents)
	    if (f instanceof Figure)
		f.show(paper)
    }
}

////////////////////////////////////////////////////////////
// Point

/** Representing a group of figures
 * @extends BoxedFigure
 */
class Point extends BoxedFigure {
    /** Creates a labeled point located at the origin. 
     * @constructor 
     * @param {String} label
     */
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
	/** coordinates of the point */
	this.xy = [0, 0]
    }

    //------------------------------------------------------------
    // Figure implementation

    /** Makes functional copy of a point.
     * @return {Point} a copy of a caller
     */
    copy () {
	// tested
	const res = new Point()
	Object.assign(res.options, this.options)
	res.xy = this.xy.copy()
	return res
    }

    /** General linear transformation. Returns a copy of the point.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Point} a copy of a caller
     */
    transform (T) {
	// tested
	const res = this.copy()
	res.xy = trans(T, res.xy)
	return res
    }

    /** Returns <tt>true</tt> for points with the same coordinates
     * @param {Any} obj
     * @return {bool}
     */
    isEqual (obj) {
	// tested
	return (obj instanceof Point) &&
	    equal(this.xy, obj.xy)
    }

    /** The refference point (is equal to a point).
     * @type {Point}
     */
    get refPoint () { return point(this.xy) }

    /** Transformation. Locates a point in a given position.
     * @return {Point} a copy of a caller
     * @param {Point | XY} pos
     */
    at (pos) {
	const res = this.copy()	
	res.xy = pos instanceof Point ? pos.xy : pos
	return res
    }
    
    //------------------------------------------------------------
    // Point methods

    /** Isomorphism for points and coordinates
     * @static
     * @param {Point | XY} obj
     * @return {Object} contains both the point and coordinates.
     * @example
     * Point.iso([2,3])
     * > {point: Point(...), xy: [2, 3]}
     * Point.iso(point([2,3]))
     * > {point: Point(...), xy: [2, 3]}
     */
    static iso (pos) {
	console.assert(isPosition(pos),
		       `Point.iso: position shoud be either Point or coordinates, got ${pos}.`)
	if (pos instanceof Point)
	    return {point: pos, xy : pos.xy}
	else
	    return {point: point(pos), xy:pos}
    }

    /** The x-coordinate of a point
     * @type {Number}
     */
    get x () { return this.xy[0] }

    /** The y-coordinate of a point
     * @type {Number}
     */
    get y () { return this.xy[1] }
        
    /** The point on a given curve specified by the curve parameter.
     * @return {Point}
     * @param {Curve} curve 
     * @param {Number} [s = 0] the internal parameter of the curve
     * @see Curve
     */
    on (curve, s = 0) {
	// tested
	console.assert(curve instanceof Curve,
		       'Point could be found on a Curve instance.')
	return this.at(curve.point(s).xy)
    }

    /** Returns a point on a segment between two points, 
     * which divides the segment at a given ratio.
     * @return {Point}
     * @param {Point | XY} pos1
     * @param {Point | XY} pos2
     * @param {Number} [s = 1/2] the internal parameter of the segment
     */
    between (pos1, pos2, s = 1/2) {
	return this.on(line(pos1, pos2), s)
    }

    /** Returns a point C specified by two points A and B and two angles
     * ∠BAC, and ∠ABC
     * @return {Point}
     * @param {Point | XY} A
     * @param {angle} BAC
     * @param {Point | XY} B
     * @param {angle} ABC
     * @example
     * // the point at the vertex or the right triangle
     * // with catet given by points (1,2) and (4,3)
     * > let A = new Point([1,2])
     * > let B = new Point([4,3])
     * > let C = new Point().azimuth(A,60,B,90)
     * > angle(A,B,C).value
     * 90
     * > angle(B,A,C).value
     * 60
     * > angle(B,C,A).value
     * 30
     */
    azimuth (pos1, angle1, pos2, angle2) {
	let s = line(pos1, pos2),
	    s1 = line(pos1, s.angle(0) + angle1),
	    s2 = line(pos2, s.angle(0) + 180 - angle2),
	    is = s1.intersections(s2)
	
	if (is.length == 0)
	    return this.at([1/0,1/0])
	else
	    return this.at(s1.intersections(s2)[0].xy)
    }
    
    /** Returns a distance between the point and an object
     * @return {Number} nonnegative number
     * @param {Figure} obj
     */
    distance (obj) {
	// tested
	if (obj instanceof Point)
	    return vector(this, obj).norm()
	if (obj instanceof Curve && obj.isContaining(this))
	    return 0
	if (obj instanceof Line) {
	    var h = new Segment().at(this).heightTo(obj)
	    if (obj.isContaining(h.end))
		return h.length
	    else if (obj instanceof Ray)
		return this.distance(obj.start)
	    else
		return [obj.start, obj.end].map(p => this.distance(p)).min()
	}
	if (obj instanceof Polygon)
	    return obj.sides.map(s => this.distance(s)).min()
	if (obj instanceof Circle)
	    return abs(this.distance(obj.center)-obj.R)
	return undefined
    }
    
    /** Sets or returns the offset for a label of a point
     * @return {Point} 
     * @param {XY} offset as a vector, pointing from the point to a label pivot
     */
    labelOffset (offset) {
	return this.option('labelOffset', offset.normalize().scale(1.2))
    }

    /** Predicate. Returns <tt>true</tt> if the point is visible on the plane.
     * @type {bool} 
     */
    get isVisible () {
	// tested
	return this.isEnclosedBy(plane)
    }

    /** Predicate. Returns <tt>true</tt> if the point lies inside a closed curve.
     * @return {bool} 
     */
    isEnclosedBy (curve) {
	// tested
	return curve.isEnclosing(this)
    }

    /** Predicate. Returns <tt>true</tt> if the point lies on a closed curve.
     * @return {bool} 
     */
    isContainedBy (curve) {
	// tested
	return curve.isContaining(this)
    }
    
    /** Sets visibility flag of the point.
     * @return {Point}
     * @param {bool} [flag = true]
     */
    invisible (f = true) {
	return this.option('invisible', f)
    }

    get box () { return [this.xy, this.xy] }
    
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

    toString() {
	return `<Point(${this.xy.map(fmt.fixed3)})>`
    }
}

////////////////////////////////////////////////////////////
// Curve
/** Abstract class representing a parameterized curve
 * @extends BoxedFigure
 */
class Curve extends BoxedFigure {
    /** Returns a curve 
     * @constructor
     */
    constructor (label) {
	super(label)

	/** Represents a type of a curve. Possible values <tt>{'infinite', 'finite', 'closed'}</tt>.
	 * @type {String} 
	 * @default 'finite'
	 */
	this.type = 'finite'

	/** Returns <tt>true</tt> for a point which lies on a curve
	 * @method
	 * @param {Point | XY} s
	 * @return {bool}
	 */
	this.equation = undefined
	/** returns a length of a curve */
	this.length = undefined
	/** An isomorphim between points on a curve and parameter. 
	 * Is used by methods [<b>point</b>]{@link Curve#point} 
	 * and [<b>locus</b>]{@link Curve#locus}
	 * @method
	 * @param {Point | Number} s
	 * @return {Object} an object with fields <tt>point</tt> and <tt>locus</tt>
	 */
	this.iso = undefined
    }

    // ------------------------------------------------------------
    // Figure implementation

    //------------------------------------------------------
    /** Resets methods [<b>iso</b>]{@link Curve#iso}, [<b>equation</b>]{@link Curve#equation}
     * and property [<b>length</b>]{@link Curve#length}. 
     * Should be callen after changing any of internal curve parameters.
     * @return {Curve} instantiated by a calee
     */
    resetEquations () {
	// sets parametrization, equation and length
	console.error('resetEquations is not implemented')
    }

    /** Helper function for defining isomorphisms.
     * @return {Function} 
     * @param {Function} pointF s -> point
     * @param {Function} locusF point -> s
     */
    static isomorphism (pointF, locusF) {
	return x => {
	    console.assert(Number.isFinite(x) || isPosition(x),
			  `Curve.iso: expected number or a point! Got ${x}`)
	    return (isPosition(x)
		    ? {point : point(x), locus : locusF(x)}
		    : {point : point(pointF(x)), locus : x})
	}
    }
    
    /** Returns a point on a curve for a given parameter 
     * @method
     * @param {Number} s
     * @return {Point}
     */
    point (s) {
	return this.iso(s).point
    }


    /** Returns internal position of a point on a curve, as a parameter value.
     * @return {Number}
     * @param {Point | XY} pos
     */
    locus (pos) {
	return this.iso(pos).locus
    }

    /** Returns a list of points where the curve intersects another curve.
     * @return {Array<Point>}
     * @param {Curve} obj
     */
    intersections (obj) {
	console.assert(obj instanceof Curve,
		       "Intersections could be found only for curves.")
	return undefined
    }

    /** Returns a curve with opposite parameterization.
     * @type {Curve}
     */
    get flip () {
    	console.error('flip is not implemented')
    }
    
    //------------------------------------------------------

    /** Returns a closed curve copy.
     * @return {Curve}
     */
    closed () {
	let res = this.copy()
	res.type = 'closed'
	res.resetEquations()
	return res
    }

    /** Returns a unit vector, tangent to a curve at a given locus.
     * @param {Number} s
     * @return {XY}
     */
    tangentV (s) {
	var df = differential(t => this.point(t).xy)
	return df(s).normalize()
    }

    /** Returns a line, tangent to a curve at a given locus. 
     * The refference point of a line coinsides with the locus.
     * @param {Number} s
     * @return {Line}
     */
    tangent (s) { return new Line().at(this.point(s), this.tangentV(s)) }
    
    /** Returns a unit vector, normal to a curve at a given locus. 
     * @param {Number} s
     * @return {XY}
     */
    normalV (s) { return rot90(this.tangentV(s).normalize()) }

    /** Returns a line, normal to a curve at a given locus. 
     * The refference point of a line coinsides with the locus.
     * @param {Number} s
     * @return {Line}
     */
    normal (s) { return new Line().at(this.point(s), this.normalV(s)) }
    
    /** Returns an angle of the tangent line at a given locus. 
     * @param {Number} s
     * @return {angle}
     */
    angle (s) {	return angleV(this.tangentV(s)) }

    /** Predicate. Returns <tt>true</tt> if point belongs to a curve.
     * @param {Point | XY} pos
     * @return {bool}
     */
    isContaining (pos) { return this.equation(pos) }

    /** Predicate. Returns <tt>true</tt> if point lies inside a closed curve.
     * @param {Point | XY} pos
     * @return {bool}
     */
    isEnclosing (pos) {
	return ray(pos).intersections(this).length % 2 == 1
    }

    /** Predicate. Returns <tt>true</tt> if the curve intersects another one.
     * @param {Curve} obj
     * @return {bool}
     */
    isIntersecting (obj) {
	return obj instanceof Curve
	    && this.intersections(obj)
	    .some(p => this.isContaining(p) && obj.isContaining(p))
    }

    /** Predicate. Returns <tt>true</tt> if the curve is closed.
     * @type {bool}
     */
    get isClosed() { return this.type == 'closed' }

    /** Predicate. Returns <tt>true</tt> if the curve is finite.
     * @type {bool}
     */
    get isFinite() { return this.type == 'finite' }

    /** Returns a point on the curve which is closest to a given figure. 
     * @param {Figure} obj
     * @return {Point}
     */
    pointClosestTo (obj){
	return this.point(findMinimum(x => obj.distance(this.point(x)),0,1).minimum)
    }

    /** Returns the refference point of the curve, corresponding to a zero parameter value.
     * @type {Point}
     */
    get refPoint () { return this.point(0) }

    /** Returns the point on the curve, corresponding to a zero parameter value.
     * @type {Point}
     */
    get start () { return this.point(0) }

    /** Returns the group of segments enclosed in the curve. 
     * @param {Curve} curve
     * @return {Group}
     */
    clipBy (curve) {
	if (curve.isClosed) {
	    let is = this.intersections(curve)
	    var parts = is.zipWith(is.tail(), segment)
	    return new Group(parts.filter(p => curve.isEnclosing(p.middle)))
	}
	return new Group([this])
    }

}

////////////////////////////////////////////////////////////
// Line

/** Representing a line
 * @extends Curve
 */
class Line extends Curve {
    /** Creates a labeled line equal to Ox
     * @constructor 
     * @param {String} label
     */
    constructor(label) {
	super(label)
	const defaults = {
	    'points': false,
	    'arrow' : false, 
	    'arrowPosition' : 'automatic',
	    'mark' : 0, 
	    'markPosition' : 'automatic',
	    'point-style': style.point,
	    'line-style': style.line,
	    'labelPosition': 'automatic',
	    'labelOffset' : 1.25
	}
	this.options = Object.assign(defaults, this.options)
	/** @default 'infinite' */
	this.type = 'infinite'
	/** The refference point of a line
	 * @type {XY}
	 */
	this.pivot = [0, 0]
	/** The refference unit vector of a line
	 * @type {XY}
	 */
	this.vector = [1, 0]
	/** The internal scale of a line
	 * @type {Number} 
	 */
	this.unit = 1
	/** @default 'infinite' */
	this.length = Infinity
	this.resetEquations()	
    }
    
    // ------------------------------------------------------------
    // Figure implementation

    /** @inheritdoc */
    copy () {
	// tested
	const res = new Line()
	Object.assign(res.options, this.options)
	res.pivot = this.pivot.copy()
	res.vector = this.vector.copy()
	res.unit = this.unit
	res.type = this.type
	res.resetEquations()
	return res
    }

    /** @inheritdoc */
    transform (T) {
	const p1 = this.point(0).transform(T),
	      p2 = this.point(1).transform(T)
	return this.joining(p1, p2)
    }

    /** @inheritdoc */
    isEqual (that) {
	return (that instanceof Line)
	    && equal(this.pivot, that.pivot)
	    && equal(this.vector, that.vector)
	    && equal(this.unit, that.unit)
    }

    /** Two lines are similar if they pass through the same two points
     */
    isSimilar (obj) {
	return (obj instanceof Line) &&
	    this.isContaining(obj.point(0)) &&
	    this.isContaining(obj.point(1))
    }

    // ------------------------------------------------------------
    // Curve implementation
    resetEquations () {
	let x0,y0,vx,vy,u;
	[x0,y0] = this.pivot;
	[vx,vy] = this.vector.normalize();
	u = this.unit;
	this.equation = pos => {
	    let xy = Point.iso(pos).xy
	    return equal(vy*(xy[0] - x0), vx*(xy[1] - y0))
	}
	this.iso = Curve.isomorphism(
	    s => [x0 + s*u*vx, y0 + s*u*vy],
	    p => { let v = Point.iso(p).xy.vsub(this.pivot)
		   return this.isTrivial ? 0 : v.dot(this.vector)/u })
	return this
    }

    
    tangentV (t) { return this.vector }
  
    intersectionL (line) {
	if (this.isParallelTo(line))
	    return []
	if (equal(this.pivot, line.pivot))
	    return [point(this.pivot)]
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
	    .sorted((p1,p2) => this.locus(p1) - this.locus(p2))
    }

    /** @inheritdoc */
    flip (s = 0) { return this.reflectAt(this.point(s)) }

    /** @inheritdoc */
    clipBy (curve) {
	if (curve.isClosed) {
	    let is = this.intersections(curve)
	    var parts = is.zipWith(is.tail(), segment)
	    return new Group(parts.filter(p => curve.isEnclosing(p.middle)))
	}
	return new Group([this])
    }
    
    // ------------------------------------------------------------
    // Line methods

    /** Predicate. Returns true if the line has zero refference vector.
     * @type {bool}
     */
    get isTrivial () {
	return this.vector.norm() == 0 || this.unit == 0
    }

    /** Returns a line, passing through a point with a given refference vector and unit.
     * @return {Line}
     * @param {Point | XY} pos
     * @param {XY} v
     */
    at (pos, v = this.vector, u = this.unit) {
	const res = this.copy()
	res.pivot = Point.iso(pos).xy
	res.vector = v.norm() == 0 ? v : v.normalize()
	res.unit = u
	res.resetEquations()
	return res
    }
    
    /** Returns a line, joining two points. 
     * The unit of the line is set by the distance between given points.
     * @return {Line}
     * @param {Point | XY} pos1
     * @param {Point | XY} pos2
     */
    joining(pos1, pos2) {
	let v = vector(pos1, pos2)
	return this.at(pos1, v, v.norm())
    }

    through(pos1) {
	return this.joining(this.start, pos1)
    }

    
    /** Returns a line, with same pivot and a given angle.
     * @return {Line}
     * @param {angle} ang
     */
    atAngle(ang) {
	// tested
	const res = this.copy()
	res.vector = Angle.iso(ang).vector
	res.resetEquations()
	return res
    }
        
    /** Returns a line, with same pivot, parallel to a given one.
     * @return {Line}
     * @param {Line} line
     */
    parallelTo (line) {
	console.assert(line instanceof Line, "Fuction parallelTo requires a line.")
	return this.atAngle(line.angle(0))
    }
    
    /** Returns a line, superposed with a given one.
     * @return {Line}
     * @param {Line} line
     */
    along (line) {
	console.assert(line instanceof Line,
		       "Fuction along requires a line.")
	return this.at(line.point(0)).parallelTo(line)
    }

    /** Returns a line, extending any other instance of a Line class.
     * @return {Line}
     * @param {Line} line
     * @example
     * segment([1,2],[5,6]).extension // line, containing the segment
     */
    get extension () {	return new Line().along(this) }
    
    /** Returns a line, perpendicularTo a given one.
     * @return {Line}
     * @param {Line} line
     */
    perpendicularTo (line) {
	console.assert(line instanceof Line,
		       "Fuction perpendicularTo requires a line.")
	let res = this.atAngle(line.angle(0) + 90),
	    x = intersectionV(res.pivot, res.vector,
			      line.pivot, line.vector)
	return res.locus(x) < 0 ? res.flip(0) : res
    }
    
    /** Returns a bisectrisse of a given angle.
     * @return {Line}
     * @param {Angle} ang
     */
    bisectrisse (ang) {
	console.assert(ang instanceof Angle,
		       "Fuction bisectrisse requires an Angle.")
	return this.atAngle((ang.start + ang.end)/2)
    }
    
    /** Returns a middle perpendicular of a given segment.
     * @return {Line}
     * @param {Segment} segment
     */
    midPerpendicular (segment) {
	console.assert(segment instanceof Segment,
		       "Fuction midPerpendicular requires a Segment.")
	return this.at(segment.point(1/2)).perpendicularTo(segment)
    }

    /** For a given circle returns a line, starting at the point outside the circle
     * tangent to it
     * @return {Line}
     * @param {Line|Ray|Segment} line
     * @param {Number} direction {-1, 1} sets the direction, 
     * corresponding to the circle's orientation
     */
    tangentTo(circle, direction = 1) {
	console.assert(circle instanceof Circle,
		       "Fuction tangentTo requires a Circle.")
	console.assert(!circle.isEnclosing(this.refPoint) || circle.isContaining(this.refPoint),
		       "The pivot point of the tangent line is inside the circle!")
	if (circle.isContaining(this.refPoint)) {
	    let res = this.along(circle.tangent(circle.locus(this.refPoint)))
	    return direction > 0 ? res : res.flip(0)
	} else {
	    const d = segment(this.point(0), circle.center)
	    const a = -direction * rad2deg(Math.asin(circle.R/d.length))
	    return this.atAngle(d.angle(0)+a)
	}
    }

    /** Predicate. Returns <tt>true</tt> if the line is parallel to another one.
     * @return {bool}
     * @param {Line} line
     */
    isParallelTo (line) {
	return line instanceof Line
	    && !this.isTrivial
	    && !line.isTrivial
	    && equal(cross(this.vector, line.vector), 0)
	    
    }

    /** Predicate. Returns <tt>true</tt> if the line is perpendicular to another one.
     * @return {bool}
     * @param {Line} line
     */
    isPerpendicularTo (line) {
	return line instanceof Line
	    && !this.isTrivial
	    && !line.isTrivial
	    && equal(line.vector.dot(this.vector), 0)
    }

    /** Predicate. Returns <tt>true</tt> if the line is tangent to a circle.
     * @return {bool}
     * @param {Circle} circle
     */
    isTangentTo (circle) {
	return circle instanceof Circle
	    && equal(circle.center.distance(this), circle.R)
    }

    /** Predicate. Returns <tt>true</tt> if the line is CCW to another one.
     * @return {bool}
     * @param {Line} line
     */
    isCCW (line) {
	console.assert(line instanceof Line,
		       "isCCW could be computed only for lines")
	return cross(this.vector, line.vector) >= 0
    }        
    
    /** Returns the distance between a line and a object.
     * @return {Number} nonnegative number
     * @param {Point | Circle} obj
     */
    distance (obj) {
	console.assert(obj instanceof Point || obj instanceof Circle,
		      'object must be a point or a circle!')
	
	if (obj instanceof Point)
	    return obj.distance(this)

	return abs(this.distance(obj.center) - obj.R)
    }

    get box () {
	const ts = plane.intersections(this).map(p => this.locus(p)).sorted()
	if (ts.length < 2) return Box.mempty
	return Box.mappend(this.point(ts[0]).box, this.point(ts[1]).box)
    }
   
    show (paper) {
	const showSegment = s => {
	    paper.line([s.start.xy, s.end.xy], this.options)
	    if (this.isLabeled)
	    {
		let lx = this.option('labelPosition') == 'automatic'
		    ? this.locus(s.end) - 2/this.unit
		    : this.option('labelPosition')
		let lpos = this.point(lx).xy.vadd([-0.5, -0.5]),
		    loff = this.normalV(0).scale(this.option('labelOffset'))
		mkLabel(paper, this.option('label'), lpos.vadd(loff))
	    }
	    if (this.option('arrow'))
	    {
		let x = this.option('arrowPosition') == 'automatic'
		    ? this.locus(s.end) : x
		paper.arrow([this.point(x - 0.1).xy, this.point(x).xy], this.options)
	    }
	    if (this.option('mark') > 0)
	    {
		for (let i = 0; i < this.option('mark'); i++) {
		    let mp =this.option('markPosition') == 'automatic'
			? s.middle
			: this.point(this.option('markPosition'))
		    let m = mp.xy.vadd(this.tangentV(0).scale(0.2*i))
		    paper.line([m.vadd(this.normalV(0).scale(0.3)), 
				m.vadd(this.normalV(0).scale(-0.3))], 
			       {'line-style': style.mark})
		}
	    }
	}

	this.clipBy(plane).contents.forEach(showSegment)
    }

    toString() {
	return `<Line (${this.pivot}) (${this.point(1).xy})>`
    }

    /** Sets the label position on the line, as a parameter value.
     * @return {Line} 
     * @param {Number} 
     */
    labelPosition (x) {
	return this.option('labelPosition', x)
    }

    /** Toggles the arrow on the line.
     * @return {Line} 
     * @param {bool} [flag = true]
     */
    arrow (f = true) {
	return this.option('arrow', f)
    }

    mark (n = 1) {
	return this.option('mark', n)
    }
    
}

/** Representing a line
 * @extends Line
 */
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
	res.unit = this.unit
	res.resetEquations()
	return res
    }
    
    isEqual (obj) {
	return (obj instanceof Ray)
	    && super.isEqual(obj)
    }

    isSimilar (obj) {
	return (obj instanceof Ray)
	    && this.start.isEqual(obj.start)
	    && equal(this.tangentV(0), obj.tangentV(0))
    }

    // ------------------------------------------------------------
    // Curve implementation

    isContaining (pos) {
	return super.isContaining(pos)
	    && this.locus(pos) >= 0
    }

    clipBy (curve) {
	if (curve.isClosed) {
	    let is = [this.start, this.intersections(curve)].flat()
	    var parts = is.zipWith(is.tail(), segment)
	    return new Group(parts.filter(p => curve.isEnclosing(p.middle)))
	}
	return new Group([this])
    }

    // ------------------------------------------------------------
    // Line implementation

    
    // ------------------------------------------------------------
    // Ray methods

    toString() {
	return `<Ray (${this.pivot}) (${this.point(1).xy})>`
    }
    
    // ------------------------------------------------------------
    // BoxedFigure implementation 

    get box () {
	let t = this.locus(this.intersections(plane)[0])
	return Box.mappend(this.point(0).box, this.point(t).box)
    }
}

/** Representing a segment
 * @extends Line
 */
class Segment extends Line {
    constructor(label) {
	super(label)
	const defaults = {
	    'labelPosition' : 1/2, 
	    'labelOffset' : 1.25
	}
	this.type = 'finite'
	this.options = Object.assign(defaults, this.options)
	this.resetEquations () 
    }

    // ------------------------------------------------------------
    // Figure implementation
    copy () {
	// tested
	const res = new Segment()
	Object.assign(res.options, this.options)
	res.pivot = this.pivot.copy()
	res.vector = this.vector.copy()
	res.unit = this.unit
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
	this.length = this.unit
	return this
    }
    
    isContaining (pos) {
	let s = this.locus(pos)
	return super.isContaining(pos)
	    && (s > 0 || equal(s, 0))
	    && (s < 1 || equal(s, 1))
    }

    // ------------------------------------------------------------
    // Segment methods
   
    /** The end point of a segment 
     * @type {Point}
     */
    get end () { return this.point(1) } 

    /** The middlepoint of a segment 
     * @type {Point}
     */
    get middle () { return this.point(1/2) }

    /** Extends the segment along it's direction, using inner units
     * @return {Segment}
     * @param {Number} s1
     * @param {Number} s2
     */
    extend (s1, s2 = null) {
	if (!s2)
	    return this.joining(this.point(0), this.point(s1))
	else
	    return this.joining(this.point(s1), this.point(s2))
    }

    /** Extends the segment along it's direction, to get a given length
     * @return {Segment}
     * @param {Number} l
     */
    extendToLength (l) {
	return this.isTrivial ? this : this.extend(0, l/this.unit)
    }

    /** Extends the segment along it's direction, until it intersects given curve.
     * If there is no intersection the ray is formed
     * @return {Segment | Ray}
     * @param {Curve} line
     */
    extendTo (obj) {
	console.assert(obj instanceof Curve,
		       "Segment could be extended only to a curve!")
	if (obj instanceof Curve) {
	    let r = ray(this.start, this.end),
		int = r.intersections(obj)
	    if (int.length == 0)
		return r
	    else
		return this.joining(r.refPoint, int[0])
	}
	return this
    }
    
    /** Returns a height  to a given line with current starting point.
     * If there is no intersection the ray is formed.
     * @return {Segment | Ray}
     * @param {Line|Ray|Segment} line
     */
    heightTo (line) {
	console.assert(line instanceof Line,
		       "Height could be built only to a line, ray or segment!")
	if (line.isContaining(this.refPoint))
	    return this.perpendicularTo(line).extend(0,0)
	return this.perpendicularTo(line).extendTo(line)
    }

    /** For a given circle returns a segment, starting at the point outside the circle
     * tangent to it. If the starting point lies inside the circle, 
     * the initial segment is returned.
     * @return {Segment}
     * @param {Line|Ray|Segment} line
     * @param {Number} direction {-1, 1} sets the direction, 
     * corresponding to the circle's orientation
     */
    tangentTo(circle, direction = 1) {
	// tested
	const d = this.start.distance(circle.center)
	const t = new Ray().at(this.start).tangentTo(circle, direction)
	switch (compare(d, circle.R)) {
	case 'EQ' : return this.parallelTo(t)
	case 'LT' : return this
	case 'GT' : 
	    const p = this.parallelTo(t).extendToLength(sqrt(d**2-circle.R**2)).end
	    return this.joining(this.start, circle.point(circle.locus(p)))
	}
	return this
    }

    /** Returns a shortest segment from the refference point to a given figure.
     * @return {Segment}
     * @param {Point|Curve} obj 
     */
    shortestTo (obj) {
	let s = this.start
	
	if (obj instanceof Point)
	    return this.joining(s, obj)

	if (obj instanceof Line) {
	    let e, h = new Segment().at(s).heightTo(obj)
	    if (obj.isContaining(h.end))
		e = h.end
	    else if (obj instanceof Ray)
		e = obj.start
	    else
		e = [obj.start, obj.end].minBy(p => this.distance(p))
	    return this.joining(s, e)
	}

	if (obj instanceof Polygon)
	    return obj.sides.map(s => this.shortestTo(s)).minBy(x => x.length)

	if (obj instanceof Circle)
	    return this.joining(s, obj.point(obj.locus(s)))

	return this
    }

    /** @inheritdoc */
    flip (s = 1/2) { return super.flip(s) }

    /** @inheritdoc */
    get box () {
	const ts = plane.intersections(this).map(p => this.locus(p)).sorted()
	if (ts.length < 2) return Box.mempty
	return Box.mappend(this.point(max(0,ts[0])).box, this.point(min(1,ts[1])).box)
    }

    /** Returns a group of segments lying inside of the given curve
     * @return {Group}
     * @param{Curve} curve
     */
    clipBy (curve) {
	if (curve.isClosed) {
	    let is = [this.start, this.intersections(curve), this.end].flat()
	    var parts = is.zipWith(is.tail(), segment)
	    return new Group(parts.filter(p => curve.isEnclosing(p.middle)))
    	}
	return new Group([this])
    }
        
    /** @inheritdoc */
    show (paper)
    {
	super.show(paper)
    }

    /** @inheritdoc */
    toString() {
	return `<Segment (${this.pivot}) (${this.point(1).xy})>`
    }
    
    mark (n) { return this.option('mark', n) }
    arrow () { return this.option('arrow', true) }
    points (t) { return this.option('points', t) }
}

////////////////////////////////////////////////////////////
// Angle

/** Representing an angle
 * @extends BoxedFigure
 */
class Angle extends BoxedFigure {
    constructor (value = 'value') {
	super(value.toString())
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

    isEnclosing (pos) {
	return (isPosition(pos)
		&& new Angle().between(this.startRay, ray(this.vertex, pos)).value < this.value)
    }

    isContaining (a) {
	return (Number.isFinite(a)
		&& cross(Angle.iso(this.start).vector, Angle.iso(a).vector) >= 0
		&& cross(Angle.iso(a).vector, Angle.iso(this.end).vector) >= 0)
    }

    on (line) {
	//tested
	const res = this.copy()
	res.vertex = point(line.pivot)
	res.start = line.angle(0)
	res.end = res.start + this.value
	return res
    }

    get refPoint () { return this.vertex }

    through (pos) {
	const res = this.copy()
	res.end = Angle.iso(vector(this.vertex, pos)).angle
	return res
    }
    
    at (pos) {
	const res = this.copy()
	res.vertex = Point.iso(pos).point
	return res
    }

    /** General linear transformation. Returns a copy of the angle.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Angle} a copy of a caller
     */
    transform (T) {
	//tested
	const res = this.copy()
	res.vertex = this.vertex.transform(T)
	res.start = this.startRay.transform(T).angle(0)
	res.end = this.endRay.transform(T).angle(0)
	return res
    }

    /** Isomorphism between angle and unit vector
     * @static
     * @param {angle | XY} obj
     * @return {Object} contains both the angle and the vector.
     * @example
     * Angle.iso([0,1])
     * > {vector: [0,1], angle: 90}
     * Angle.iso([1,1])
     * > {vector: [1,1], angle: 45}
     * Angle.iso(45)
     * > {vector: [1,1], angle: 45}
     * Angle.iso(270)
     * > {vector: [0,-1], angle: 270}
     */
    static iso (x) {
	console.assert(Number.isNaN(x) || Number.isFinite(x) || isXY(x),
		       `Angle.iso: angle shoud be either number or a vector, got ${x}`)

	if (isXY(x)) {
	    if (equal(x, [0,0]))
		return {vector : x, angle : NaN}
	    else {
		let nv = x.normalize()
	    	return {vector : nv, angle : angleV(nv)}
	    }
	}
	if (Number.isNaN(x))
	    return {vector : [0, 0], angle : NaN}
	let phi = deg2rad(x)
	return {vector : [cos(phi), sin(phi)], angle : x}
    }

    static azimuth (pos1, pos2) {
	return Angle.iso(vector(pos1, pos2)).angle
    }
    
    get value () { return (2*360 + this.end - this.start) % 360 }
    get startRay () {
	return new Ray().at(this.vertex).atAngle(this.start)
    }
    get endRay () {
	return new Ray().at(this.vertex).atAngle(this.end)
    }
    get isRight () {
	return equalMod(360, this.value, 90) || abs(cos(deg2rad(this.value))) < 1e-14
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
    
    within (pos1, pos2, pos3) {
	// tested
	const res = this.copy()
	res.start = ray(pos2, pos1).angle(0)
	res.end = ray(pos2, pos3).angle(0)
	res.vertex = pos2
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
	} else {
	    
	    for(let i = 0; i < this.options['strokes']; i++)
		p.arc(this.vertex.xy, 2-i*0.3, this.start, this.end, this.options)
	}
	
	let vs = Angle.iso(this.start).vector.scale(3),
	    ve = Angle.iso(this.end).vector.scale(3)
	p.line([this.vertex.xy.vadd(vs), this.vertex.xy, this.vertex.xy.vadd(ve)], this.options)
	
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

/** Representing a polygon or a polyline
 * @extends Curve
 */
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

    /** General linear transformation. Returns a copy of the polygon.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Polygon} a copy of a caller
     */
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

    isContaining (pos) {
	return this.sides.some(s => s.isContaining(pos)) 
    }

    locus (pos) {
	let i = this.sides.map(s => s.locus(pos)).findIndex(l => 0 <= l && l <= 1)
	console.assert(i != -1,
		       `Point [${pos.xy}] can't be located on a curve ${this.label()}.`)
	let s = this.sides[i],
	    l0 = this.verticesLocations[i],
	    l1 = s.locus(pos)*s.length/this.length
	return l0 + l1
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

    addVertex (pos, n = this.number) {
	var res = this.copy()
	res.vertices.insert(n, Point.iso(pos).point)
	res.resetEquations()
	return res
    }
    
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
	if (!this.isClosed) 
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
   
    at (pos, vertex_n = 1) {
	return this.superpose(this.vertex(vertex_n), Point.iso(pos).point)
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

    parametric (fn, domain) {
	return this.through(domain.map(s => point(fn(s))))
    }

    polar (fn, n = 50, start = 0, end = 2*pi) {
	let step = (end - start)/n
	return this.parametric(t => [cos(t), sin(t)].scale(fn(t)),
			       range(start, end+step, step))
    }

    circleSegment (r = 1, start = 0, end = 2*pi) {
	return this.polar(t => r, (end - start)/(2*pi/100), start, end)
    }

    sector (r = 1, start = 0, end = 2*pi) {
	return this.circleSegment(r, start, end)
    }

    regular (n, r = 1) {
	return this.polar(t => r, n).rotate(90).closed()
    }

    chart (data) {
	let pts = isXY(data[0]) ? data : range(data.length).zip(data)
	return this.through(pts.map(point))
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

/** Representing a triangle
 * @extends Polygon
 */
class Triangle extends Polygon {
    constructor (pos1 = null, pos2 = null, pos3 = null) {
	super()
	const defaults = { }
	this.options = Object.assign(defaults, this.options)
	let s
	if (!pos1) {
	    s =  new Polygon().regular(3)
	} else if (!pos2) {
	    s =  new Polygon().regular(3).at(pos1)
	} else if (!pos3) {
	    s = new Polygon().regular(3).on(segment(pos1,pos2))
	} else {
	    s = new Polygon().through([pos1, pos2, pos3])
	}
	this.vertices = s.vertices
	this.type = 'closed'
	return this.resetEquations()
    }

    copy () { return super.copy(new Triangle()) }

    get isTrivial () {
	return this.sides.some(s => equal(s.length, 0))
	    || (range(3).map(i => this.vertexAngle(i))
		.some(a => equalMod(180, a, 0)))
    }
    
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
	return new Segment().at(v).heightTo(s)
    }

    bisectrisse (i) {
	const v = this.vertex(i),
	      a = this.vertexAngle(i),
	      s = this.side(i+1)
	return new Segment().at(v).bisectrisse(a).extendToLine(s)
    }
}

/** Representing a quadrilateral polygon
 * @extends Polygon
 */
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

    diagonal (i) { return segment(this.vertex(i),this.vertex(i+2)) }
}

/** Representing a square
 * @extends Quadrilateral
 */
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

/** Representing a rectangle
 * @extends Quadrilateral
 */
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

////////////////////////////////////////////////////////////
// Circle
/** Representing a circle
 * @extends Curve
 */
class Circle extends Curve {
    constructor(r = 1) {
	super()
	const defaults = {
	    'style': style.circle
	}
	this.options = Object.assign(defaults,this.options)
	this.type = 'closed'
	this.center = origin.copy()
	this.R = abs(r)
	this.phase = 0
	this.orientation = 1
	this.resetEquations()
    }

    //------------------------------------------------------------
    // Figure implementation
    copy () {
	const res = new Circle(this.R)
	Object.assign(res.options, this.options)
	res.R = abs(this.R)
	res.center = this.center.copy()
	res.phase = this.phase
	res.orientation = this.orientation
	res.resetEquations()
	return res
    }

    /** General linear transformation. Returns a copy of the circle. 
     * Scaling and stretching does not affect the circle shape. 
     * To make a fully transformable circle use [<b>polar</b>]{@link Polygon#polar}.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Circle} a copy of a caller
     */
    transform (T) {
	const r = this.radius(0).transform(T)
	const t = this.tangent(0).transform(T)
	const res = new Circle()
	res.R = r.length
	res.center = r.start
	res.phase = res.locus(r.end)
	res.orientation = Math.sign(cross(r.vector, t.vector))
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

    get isTrivial () {	return equal(this.R, 0)  }
    
    get refPoint () { return this.center }
    
    //------------------------------------------------------------
    // Curve implementation
   
    resetEquations () {
	const c = this.center,
	      r = this.R,
	      ph = this.phase,
	      w = this.orientation
	this.length = 2*this.R*pi
	this.equation = xy => equal(point(xy).distance(this.center), this.R)
	this.iso = Curve.isomorphism(
	    t => [c.x + r*cos(2*pi*(w*t + ph)),
		  c.y + r*sin(2*pi*(w*t + ph))],
	    p => deg2rad(line(this.center, p).angle(0))/(2*pi))
    }
    
    tangentV (t) {
	const ph = this.phase,
	      w = this.orientation
	return [-w*sin(2*pi*(w*t + ph)), w*cos(2*pi*(w*t + ph))]
    }
    
    isEnclosing (pos) {
	return Point.iso(pos).point.distance(this.center) <= this.R
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
	if (this.isTrivial || line.isTrivial) return []
	if (line.extension.isContaining(this.center)) {
	    let c = line.locus(this.center),
		r = this.R/line.unit
	    return [line.point(c - r), line.point(c + r)]
	}
	let H = new Line().at(this.center)
	    .perpendicularTo(line)
	    .intersections(line.extension)[0],
	    d = H.distance(this.center),
	    res
	switch (compare(d, this.R)) {
	case 'GT' : res = []; break
	case 'EQ' : res = [H]; break;
	case 'LT' :
	    let x = sqrt(this.R**2 - d**2)/line.unit, xh = line.locus(H)
	    res = [line.point(xh-x), line.point(xh+x)] 
	}
	return res.filter(p => line.isContaining(p) && this.isContaining(p))
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
	    .sorted((p1,p2) => this.locus(p1) - this.locus(p2))
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
	return range(0,1,1/4).foldMap(t => this.point(t).box, Box)
    }
    
    show (paper) {
	paper.disk(this.center.xy, this.R, this.options)
    }

    toString () {
	return `<Circle (${this.center.xy}) ${this.R}>`
    }
}

////////////////////////////////////////////////////////////
// Text
/** Representing a text ona chart
 * @extends BoxedFigure
 */
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

    get refPoint () { return this.pivot }
    
    at (pos) {
	const res = this.copy()
	res.pivot = Point.iso(pos).point
	return res
    }

    on (line, t = 0) {
	let res = this.copy()
	res.pivot = line.point(t)
	return res.atAngle(line.angle(0)).option('labelOffset',line.normalV(0).scale(2/3))
    }

    /** General linear transformation. Returns a copy of the text. 
     * Does not affect the shape of letters, changing only position and orientation.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Text} a copy of a caller
     */
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
/** Representing a group scale on a curve
 * @extends BoxedFigure
 */
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

    /** General linear transformation. Returns a copy of the scale.
     * @param {Matrix} T the 3×3 transformation matrix
     * @return {Scale} a copy of a caller
     */
    transform (T) {
	const res = this.copy()
	res.curve = this.curve.transform(T)
	return res	
    }

    get refPoint () { return this.curve.point(0) }
    
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
	    let s = new Segment().at(p)
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

/** Representing a chart */
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
	let cnt = [...arguments].flat()
	for (let o of cnt)
	    if (o instanceof Figure)
		o.show(this.paper)
	return this
    }
}

////////////////////////////////////////////////////////////
// smart constructors

/** Smart point constructor, returns anonymous point.
* @memberof Global
* @return {Point}
* @param {Point | XY} pos the point position
* @example
* point([1, 2]).xy
* > [1,2]
* @example
* // Properties
* // idempotent
* property(Any.Point)( A => point(point(A)).isEqual(point(A)) )
*/
function point (pos) {
    return new Point().at(pos)
}

/** Smart line constructor, returns anonymous line.
* @memberof Global
* @return {Line}
* @param {Point | XY} pos1 the first point of a line
* @param {Point | XY | angle} [pos2 = 0] the second point or the angle of a line
* @example
* line([1, 2], [3, 4]) // passes through two points
* line(origin, [3, 4]) // passes through two points
* line([1, 2], 30) // passes through a point by 30°
* line(30) // passes through the origin by 30°
*/
function line (pos1, pos2 = 0) {
    console.assert((isPosition(pos1) || Number.isFinite(pos1))
		   && isPosition(pos2) || Number.isFinite(pos2),
		   "Line must be specified by a position or an angle.")
    if (isPosition(pos1) && isPosition(pos2))
	return new Line().joining(pos1, pos2)
    if (isPosition(pos1) && Number.isFinite(pos2))
	return new Line().at(pos1).atAngle(pos2)
    if (Number.isFinite(pos1))
	return new Line().atAngle(pos1)
}

/** Smart ray constructor, returns anonymous ray.
* @memberof Global
* @return {Line}
* @param {Point | XY} pos1 the starting point of a ray
* @param {Point | XY | angle} [pos2 = 0] the second point or the angle of a ray
* @example
* ray([1, 2], [3, 4]) // starts from one point and passes through another
* ray(origin, [3, 4]) // starts from the origin and passes through a point
* ray([1, 2]) // starts from a point with anle 0°
* ray([1, 2], 30) // starts from a point by 30°
* ray(30) // starts from the origin by 30°*/
function ray (pos1, pos2 = 0) {
    console.assert(isPosition(pos1) || Number.isFinite(pos1)
		   && isPosition(pos2) || Number.isFinite(pos2),
		   "Ray must be specified by a position or an angle.")
    return new Ray().along(line(pos1, pos2))
}

/** Smart segment constructor, returns anonymous segment,
* joining through two given points.
* @memberof Global
* @return {Line}
* @param {Point | XY} pos1 the start point of a segment
* @param {Point | XY} pos2 the end of a segment
*/
function segment (a, b) {
    return new Segment().joining(a, b)
}

const angle = (a,b,c) => new Angle().within(a,b,c)

/** Smart circle constructor, returns anonymous circle,
* with given center and radius.
* @memberof Global
* @return {Circle}
* @param {Point | XY} center 
* @param {Number} radius 
*/
function circle(c,r) {
    return new Circle(r).at(c)
}

function mkLabel(p, l, pnt, opts)
{
    let options = Object.assign({'at':pnt,'style':style.label}, opts)
    return p.label(l, options)
}

////////////////////////////////////////////////////////////
// predefined objects

const origin = new Point('O').at([0,0]).option('labelOffset',[-1,-1])
const Ox = line(origin, 0).thin().color('wheat')
const Oy = line(origin, 90).thin().color('wheat')
//gconst plane = new Square(paperSize)
const plane = new Circle(paperSize/2)
