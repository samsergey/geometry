//const anyCoordinate = () => anyNum().range(-paperSize/2, paperSize/2).precision(1).ascending()
//const anyXY = () => new Arbitrary().tuple([anyCoordinate(), anyCoordinate()])

const anyXY = () => ArbitraryCoordinates()
const anyPoint = () => args(anyXY()).iso(point, p => [p.xy])
const anyTwoPoints = () => args(anyPoint(), anyPoint())
const anyAngle = () => anyNum().range(0,360).precision(1)
const anyLine = () => args(anyPoint(), anyPoint()).iso(line, l => [l.point(0), l.point(1)])
const anySegment = () => args(anyPoint(), anyPoint()).iso(segment, l => [l.point(0), l.point(1)])
const anyRay = () => args(anyPoint(), anyAngle()).iso(ray, r => [r.start, r.angle(0)])
const anyRadius = () => anyNum().range(1, paperSize/4).precision(1).ascending()
const anyCircle = () => args(anyPoint(), anyRadius()).iso(circle, c => [c.center, c.R]).ascending()
const anyTriangle = () => (args(anyPoint(), anyPoint(), anyPoint())
			   .iso((p1,p2,p3) => new Triangle(p1,p2,p3), c => c.vertices)
			   .filter(t => t.isNonTrivial))

////////////////////////////////////////////////////////////
const testTests = {
    name : 'Tests',
    skip : true,
    suite : [
	{
	    skip : false,
	    name  : "1",
	    for   : [anyNum().range(0,100).precision(0.1)],
	    hold  : n => n == 4,
	    log : true
	},
	{
	    skip : false,
	    name  : "2",
	    for   : [anyNum(), anyNum()],
	    hold  : (n, m) => n >= m,
	    log : true
	},
	{
	    name  : "3",
	    for   : [anyCircle(), anyPoint()],
	    assuming : [(c, p) => c.R > 1,
			(c, p) => !c.center.isEqual(p)],
	    hold  : (c, p) => ! (c .isEnclosing (p)),
	    log : true
	}
    ]
}

////////////////////////////////////////////////////////////
const testPoints = {
    name: 'Points',
    suite : [
	{
	    name  : "isomorphism 1",
	    for   : [anyPoint()],
	    holds : p => Point.iso(Point.iso(p).xy).point .isEqual (p)
	},
	{
	    name   : "copy 1",
	    run    : () => point([3,4]).copy(),  result : point([3,4])
	},
	{
	    name  : "superpose",
	    for   : [anyPoint(), anyPoint()],
	    holds : (p1,p2) => p1.superpose(p1,p2) .isEqual (p2),
	    number : 5
	},
	{
	    name  : "isomorphism 1",
	    for   : [anyPoint()],
	    hold  : p => Point.iso(Point.iso(p).xy).point .isEqual (p)
	},
	{
	    name : "isomorphism 2",
	    for  : [anyXY()],
	    hold : xy => equal(Point.iso(Point.iso(xy).point).xy, xy)
	},
	{
	    name : "copy 1",
	    for  : [anyPoint()], hold : p => p.copy() .isEqual (p),
	    number : 5
	},
	{
	    name : "coordinates 1",
	    for  : [anyPoint()],
	    hold : p => origin.at(p) .isEqual (point(p)),
	    number : 5
	},
	{
	    name : "equality 1",
	    for  : [anyPoint()], hold : p => p .isEqual (p),
	    number : 5
	},
	{
	    name : "equality 2",
	    for  : [anyPoint()], hold : p => point(p.xy) .isEqual (p),
	    number : 5
	},
	{
	    name : "equality 3",
	    for  : [anyPoint()],  
	    hold : p => !p.copy().translate([1,1]) .isEqual (p),
	    number : 5
	},
	{
	    name : "translate 1", 
	    run : () => new Point().translate([1,2]).xy,  result : [1,2]
	}
    ]
}

////////////////////////////////////////////////////////////
const testTransformations = {
    name : 'Transformations',
    suite : [
    ]
}

////////////////////////////////////////////////////////////
const testLines = {
    name : 'Lines',
    suite : [
	{
	    name     : "isomorphism 1",
	    for      : [anyLine(), anyNum()],
	    assuming : [l => l.isNonTrivial],
	    hold     : (l, s) => equal(l.locus(l.point(s)), s),
	    number   : 10
	},
	{
	    name     : "isomorphism 2",
	    for      : [anyPoint(), anyNum()],
	    hold     : (p, s) => { let l = line(p, p)
				   return l.isTrivial && equal(l.locus(l.point(s)), 0)},
	    number   : 10
	},
	{
	    name     : "isomorphism 3",
	    for      : [anyLine(), anyNum()],
	    assuming : [l => l.isNonTrivial],
	    hold     : (l, s) => { let p = l.point(s)
	 			   return equal(l.point(l.locus(p)), p)},
	    number   : 10
	},
	{
	    name     : "equation 1",
	    for      : [anyLine(), anyNum()],
	    hold     : (l, s) => l.equation(l.point(s)),
	    number   : 10
	},
	{
	    name     : "equation 2",
	    for      : [anyLine(), anyNum()],
	    assuming : [l => l.isNonTrivial],
	    hold     : (l, s) => !l.equation(l.point(s).translate(l.normalV(0))),
	    number   : 10
	},
	{
	    skip     : true,
	    name     : "line intersections 1",
	    for      : [anyLine(), anyLine()],
	    assuming : [(a,b) => !(a.isTrivial || b.isTrivial)],
	    hold     : (a,b) => { let ps = a.intersections(b)
				  return a.isParallelTo(b) ? ps.length == 0 : ps.length == 1 }
	},
	{
	    name     : "plane intersections 1",
	    for      : [anyLine()],	     
	    hold     : l => { let ps = plane.intersections(l)
			      return ps.every(p => plane.isContaining(p)) }
	},
	{
	    name     : "perpendicularity 1",
	    for      : [anyLine(), anyXY()],
	    assuming : [l => l.isNonTrivial],
	    hold     : (l, xy) => new Line().at(xy).perpendicularTo(l).isPerpendicularTo(l)
	},
	{
	    name     : "perpendicularity 2",
	    for      : [anyLine()],
	    hold     : l => !l.isPerpendicularTo(line(origin, origin))
	},
	{
	    name     : "perpendicularity 3",
	    for      : [anyLine(), anyLine()],
	    assuming : [(l1, l2) => l1.isNonTrivial && l2.isNonTrivial],
	    hold     : (l1, l2) => { let p = l1.perpendicularTo(l2)
				     return (p.refPoint .isEqual (l1.refPoint)
					     && p.locus(p.intersections(l1)[0]) >= 0) }
	},
	{
	    name     : "parallelity 1",
	    for      : [anyLine(), anyXY()],
	    assuming : [l => l.isNonTrivial],
	    hold     : (l, xy) => new Line().at(xy).parallelTo(l).isParallelTo(l)
	},
	{
	    name     : "parallelity 2",
	    for      : [anyLine()],
	    hold     : l => !l.isParallelTo(line(origin, origin))
	},
	{
	    name     : "tangentTo",
	    for      : [anyPoint(), anyCircle()],
	    assuming : [(p, c) => !c.isEnclosing(p)],
	    hold     : (p, c) => { let t = new Line().at(p).tangentTo(c)
				   let is = c.intersections(t)
				   return (is.length == 1
					   && new Line().at(is[0]).perpendicularTo(t).isContaining(c.center))}
	},
	{name : "LineEquation",
	 suite : [
	     {name :"1",
	       for : [anyXY(), anyXY()],
	       hold : (p1,p2) => lineEquation(p1,p2)(0) . isEqual (point(p1)),
	       number : 10
	     },
	     {name :"2",
	      for : [anyXY(), anyXY()],
	      hold : (p1,p2) => lineEquation(p1,p2)(1) . isEqual (point(p2)),
	      number : 10
	     },
	     {name :"4", run : () => lineEquation([0,0],[1,2],2)(1).xy, result : [2,4]},
	     {name :"5", run : () => lineEquation([0,0],[1,2],2)(-1).xy, result : [-2,-4]},
	     {name :"6", run : () => lineEquation([0,0],[0,1])(0).xy, result : [0,0]},
	     {name :"7", run : () => lineEquation([0,0],[0,1])(2).xy, result : [0,2]},
	     {name :"8", run : () => lineEquation([1,2],[1,2])(0).xy, result : [1,2]},
	     {name :"9", run : () => lineEquation([1,2],[1,2])(1).xy, result : [1,2]},
	     {name :"10", run : () => lineEquation([1,2],[0,0],-1)(0).xy, result : [1,2]},
	     {name :"11", run : () => lineEquation([1,2],[0,0],-1)(1).xy, result : [2,4]}
	 ]},
	{name:"intersectionV 1",
	 run : () => intersectionV([0,0],[1,2],[0,0],[2,1]), result : [0,0]},
	{name:"intersectionV 2",
	 run : () => intersectionV([1,2],[1,1],[2,1],[-1,1]), result : [1,2]},
	{name:"intersectionV 3",
	 run : () => intersectionV([1,0],[0,1],[0,1],[1,0]), result : [1,1]},
	{name:"intersectionV 5",
	 run : () => intersectionV([1,2],[-1,-2],[3,4],[-3,-4]), result : [0,0]},
	{name:"intersectionV 6",
	 run : () => intersectionV([1,2],[1,2],[3,4],[2,4]), result : [1/0,1/0]},
	{name:"intersectionV 7",
	 run : () => intersectionV([1,2],[1,2],[1,2],[-2,-4]), result : [1/0,1/0]}
    ]
}

////////////////////////////////////////////////////////////
const testRays = {
    name :'Rays',
    suite : [
	{
	    name     : "isContaining 1",
	    for      : [anyRay(), anyNum()],
	    assuming : [r => r.isNonTrivial],
	    hold  : (r, s) => (s >= 0
			       ? r.isContaining(r.point(s))
			       : !r.isContaining(r.point(s)))
	},
	{
	    name     : "isContaining 2",
	    for      : [anyRay(), anyNum()],
	    assuming : [r => r.isNonTrivial],
	    hold  : (r, s) => !r.isContaining(r.point(s).translate(r.normalV(0)))
	}
    ]
}

////////////////////////////////////////////////////////////
const testSegments = {
    name : 'Segments',
    suite : [
	{
	    name     : "isContaining 1",
	    for      : [anySegment(), anyNum()],
	    assuming : [s => s.isNonTrivial],
	    hold  : (s, t) => (t >= 0 && t <= 1
			       ? s.isContaining(s.point(t))
			       : !s.isContaining(s.point(t)))
	},
	{
	    name     : "extension 1",
	    for      : [anySegment(), anyNum()],
	    hold  : (s, n) => equal(s.extend(n).length, abs(n)*s.length)
	},
	{
	    name     : "extension 2",
	    for      : [anySegment(), anyNum()],
	    hold  : (s, n) => equal(s.extend(n).extend(n).length, n*n*s.length)
	},
	{
	    name     : "extension 3",
	    for      : [anySegment(), anyNum()],
	    assuming : [(_,n) => n >= 0],
	    hold  : (s, n) => equal(s.extendToLength(n).length, s.isTrivial ? 0 : n)
	},
	{
	    name     : "extension 4",
	    for      : [anySegment()],
	    hold  : (s, n) => s.extend(0).isTrivial && s.extendToLength(0).isTrivial
	},
	{
	    name : "extendTo 1",
	    run : () => segment([1,1],[2,3]).extendTo(Ox),
	    result : ray([1,1],[2,3]),
	    log : true
	},
	{
	    name : "extendTo 2",
	    for  : [anySegment(), anyLine()],
	    hold : (s, l) => { let h = s.extendTo(l)
			       return (h instanceof Segment
				       ? l .isContaining (h.end)
				       : h instanceof Ray) }
	},
	{
	    name : "extendTo 3",
	    for  : [anyPoint(), anySegment()],
	    assuming : [ (p, s) => s.isNonTrivial,
			 (p, s) => !s.isContaining(p)],
	    hold : (p, s) => { let a = Angle.azimuth(p, s.start),
				   c = new Segment().at(p).atAngle(a).extendTo(s)
			       return (c instanceof Segment
				       && c.end .isEqual (s.start)) }
	},
	{
	    name : "extendTo 4",
	    for  : [anyPoint(), anyAngle(), anyCircle()],
	    assuming : [ (p, a, c) => c.isEnclosing(p) ],
	    hold : (p, a, c) => new Segment().at(p).atAngle(a).extendTo(c) instanceof Segment 
	},
	{
	    name : "heightTo 1",
	    for : [anySegment(), anyRay()],
	    hold :  (s, r) => { let h = s.heightTo(r)
				     return (h instanceof Segment
					     ? r .isContaining (h.end)
					     : h instanceof Ray) }
	},
	{
	    name : "heightTo 2",
	    for : [anySegment(), anyLine()],
	    hold :  (s, l) => { let h = xx[0] = s.heightTo(l)
				return (h instanceof Segment
					&& l .isContaining (h.end)) }
	}
    ]
}

var xx = []

const testGroup = {
    name : 'Group',
    suite : [
    ]
}


const testIntersections = {
    name : 'Intersections',
    suite : [
    ]
}

const testAngles = {
    name : 'Angle',
    suite : [
	{
	    name : "modulus",
	    for : [anyAngle()],
	    hold : a => equal(mod(-a, 360), mod(360-a, 360))
	},
	{
	    name : "isomorphism 1",
	    for : [anyXY()],
	    hold : v => equal(cross(Angle.iso(Angle.iso(v).angle).vector, v.normalize()), 0)
	},
	{
	    name : "isomorphism 2",
	    for : [anyAngle()],
	    hold : a => equalMod(360,Angle.iso(Angle.iso(a).vector).angle, a)
	},
	{ name : "isContaining",
	  suite : [
	      {name: "1", run : () => new Angle(30). isContaining (40), result : false },
	      {name: "2", run : () => new Angle(30). isContaining (10), result : true },
	      {name: "4", run : () => new Angle(30). isContaining (0), result : true },
	      {name: "5", run : () => new Angle(30). isContaining (30), result : true },
	      {name: "6", run : () => new Angle(30).rotate(45) .isContaining (30), result : false },
	      {name: "7", run : () => new Angle(30).rotate(45) .isContaining (70), result : true },
	      {name: "8", run : () => new Angle(30).rotate(-45) .isContaining (0), result : false },
	      {name: "9", run : () => new Angle(30).rotate(-20) .isContaining (0), result : true },
	      {name: "9", run : () => new Angle(30).rotate(340) .isContaining (0), result : true },
	      {name: "10", run : () => new Angle(30).reflectAt([0,0]) .isContaining (200), result : true },
	      {name: "11", run : () => new Angle(30).reflectAt([0,0]) .isContaining (170), result : false }
	  ]}
    ]
}

const testPolygon = {
    name : 'Polygon',
    suite : [
    ]
}

const testSquare = {
    name : 'Square',
    suite : [
    ]
}

const testCircle = {
    name : 'Circle',
    suite : [
    ]
}


var allTests = {
    name : 'all',
    suite : [
	testTests,
	testPoints,
	testLines,
	testRays,
	testSegments,
	testPolygon,
	testCircle,
	testAngles,
	testTransformations,
	testGroup,
	testIntersections
    ]
}

runTests(allTests)
console.log('Testing done')
