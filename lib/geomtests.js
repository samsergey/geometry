const labeled = l => f => (...x) => l ? f(...x).label(l) : f(...x)

const anyParam =
      (a=0, b=1) => anyNum().range(a,b).precision(0.01).ascending()
const anyXY =
      () => ArbitraryCoordinates()
const anyPoint =
      (l) => args(anyXY()).iso(labeled(l)(point), p => [p.xy])
const anyAngle =
      () => anyNum().range(0,360).precision(1)
const anyLine =
      (l) => args(anyPoint(), anyPoint()).iso(labeled(l)(line), l => [l.point(0), l.point(1)])
const anySegment =
      (l) => args(anyPoint(), anyPoint()).iso(labeled(l)(segment), l => [l.point(0), l.point(1)])
const anyRay =
      (l) => args(anyPoint(), anyAngle()).iso(labeled(l)(ray), r => [r.start, r.angle(0)])
const anyRadius =
      () => anyNum().range(1, paperSize/4).precision(1).ascending()
const anyCircle =
      () => args(anyPoint(), anyRadius()).iso(circle, c => [c.center, c.R]).ascending()
const anyTriangle =
      () => (args(anyPoint(), anyPoint(), anyPoint())
	     .iso((p1,p2,p3) => new Triangle(p1,p2,p3), c => c.vertices)
	     .filter(t => t.isNontrivial))

////////////////////////////////////////////////////////////
const testTests = {
    name : 'Tests',
    skip : true,
    log : true,
    suite : [
	{
	    name  : "1",
	    for   : [anyNum().range(0,100).precision(0.1)],
	    hold  : n => n == 4,
	    log : true,
	    number : 10
	},
	{
	    name  : "2",
	    for   : [anyNum(), anyNum()],
	    hold  : (n, m) => n >= m,
	    log : false
	},
	{
	    name  : "3",
	    for   : [anyCircle(), anyPoint()],
	    assuming : [(c, p) => c.R > 1,
			(c, p) => !c.center.isEqual(p)],
	    hold  : (c, p) => ! (c .isEnclosing (p)),
	}
    ]
}

////////////////////////////////////////////////////////////
const testPoints = {
    name: 'Points',
    number : 10,
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
	    holds : (p1,p2) => p1.superpose(p1,p2) .isEqual (p2)
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
	    for  : [anyPoint()], hold : p => p.copy() .isEqual (p)
	},
	{
	    name : "coordinates 1",
	    for  : [anyPoint()],
	    hold : p => origin.at(p) .isEqual (point(p))
	},
	{
	    name : "equality 1",
	    for  : [anyPoint()], hold : p => p .isEqual (p)
	},
	{
	    name : "equality 2",
	    for  : [anyPoint()], hold : p => point(p.xy) .isEqual (p)
	},
	{
	    name : "equality 3",
	    for  : [anyPoint()],  
	    hold : p => !p.copy().translate([1,1]) .isEqual (p)
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
	{ number : 10,
	  suite : [
	      {
		  name     : "isomorphism 1",
		  for      : [anyLine(), anyNum()],
		  assuming : [l => l.isNontrivial],
		  hold     : (l, s) => equal(l.locus(l.point(s)), s)
	      },
	      {
		  name     : "isomorphism 2",
		  for      : [anyPoint(), anyNum()],
		  hold     : (p, s) => { let l = line(p, p)
					 return l.isTrivial && equal(l.locus(l.point(s)), 0)}
	      },
	      {
		  name     : "isomorphism 3",
		  for      : [anyLine(), anyNum()],
		  assuming : [l => l.isNontrivial],
		  hold     : (l, s) => { let p = l.point(s)
	 				 return equal(l.point(l.locus(p)), p)}
	      },
	      {
		  name     : "equation 1",
		  for      : [anyLine(), anyNum()],
		  hold     : (l, s) => l.equation(l.point(s))
	      },
	      {
		  name     : "equation 2",
		  for      : [anyLine(), anyNum()],
		  assuming : [l => l.isNontrivial],
		  hold     : (l, s) => !l.equation(l.point(s).translate(l.normalV(0)))
	      }
	  ]
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
	    assuming : [l => plane.isEnclosing(l.refPoint) ],
	    hold     : l => { let ps = plane.intersections(l)
			      return ps.every(p => plane.isContaining(p)) }
	},
	{
	    name     : "perpendicularity 1",
	    for      : [anyLine('a'), anyPoint('A')],
	    with     : (l, p) => [l, p, new Line('b').at(p).perpendicularTo(l) ],
	    hold     : (l, p, nl) => nl.isPerpendicularTo(l),
	    assuming : [l => l.isNontrivial],
	},
	{
	    name     : "perpendicularity 2",
	    for      : [anyLine()],
	    hold     : l => !l.isPerpendicularTo(line(origin, origin))
	},
	{
	    name : "perpendicularity 3",
	    for : [anyLine('1'), anyLine('2')],
	    with : (l1, l2) => [ l1, l2, l1.perpendicularTo(l2).label('p') ],
	    hold : (l1, l2, p) => (p.refPoint .isEqual (l1.refPoint) &&
				   gequal(p.locus(p.intersections(l2)[0]) , 0)),
	    assuming : [(l1, l2, p) => l1.isNontrivial && l2.isNontrivial]
	},
	{
	    name     : "parallelity 1",
	    for      : [anyLine(), anyXY()],
	    assuming : [l => l.isNontrivial],
	    hold     : (l, xy) => new Line().at(xy).parallelTo(l).isParallelTo(l)
	},
	{
	    name     : "parallelity 2",
	    for      : [anyLine()],
	    hold     : l => !l.isParallelTo(line(origin, origin))
	},
	{
	    name     : "tangentTo 1",
	    for      : [anyPoint(), anyCircle()],
	    with     : (p, c) => {let t = new Line().at(p).tangentTo(c),
				      i = c.intersections(t)
				  return [p, c, t, i]},
	    hold     : (p, c, t, i) => (i.length == 1
					&& new Line().at(i[0]).perpendicularTo(t).isContaining(c.center)),
	    assuming : [(p, c) => c.isNontrivial && !c.isEnclosing(p)]
	    
	},
	{
	    name     : "tangentTo 2",
	    for      : [anyCircle(), anyParam()],
	    with     : (c, x) => [c, x, new Line().at(c.point(x)).tangentTo(c), c.radius(x)],
	    hold     : (c, x, t) => t.isPerpendicularTo(c.radius(x)),
	    assuming : [ c => c.isNontrivial ],
	    log : true
	    
	},
	{name : "LineEquation",
	 number : 10,
	 suite : [
	     {name :"1",
	       for : [anyXY(), anyXY()],
	       hold : (p1,p2) => lineEquation(p1,p2)(0) . isEqual (point(p1))
	     },
	     {name :"2",
	      for : [anyXY(), anyXY()],
	      hold : (p1,p2) => lineEquation(p1,p2)(1) . isEqual (point(p2))
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
	    assuming : [r => r.isNontrivial],
	    hold  : (r, s) => (s >= 0
			       ? r.isContaining(r.point(s))
			       : !r.isContaining(r.point(s)))
	},
	{
	    name     : "isContaining 2",
	    for      : [anyRay(), anyNum()],
	    assuming : [r => r.isNontrivial],
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
	    assuming : [s => s.isNontrivial],
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
	    assuming : [ (p, s) => s.isNontrivial,
			 (p, s) => !s.extension.isContaining(p)],
	    hold : (p, s) => { let a = Angle.azimuth(p, s.start),
				   c = new Segment().at(p).atAngle(a).extendTo(s)
			       return (c instanceof Segment
				       && c.end .isEqual (s.start)) }
	},
	{
	    name : "extendTo 4",
	    for  : [anyPoint(), anyAngle(), anyCircle()],
	    assuming : [ (p, a, c) => c.isEnclosing(p),
			 (p, _, c) => c.isNontrivial ],
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
	    for : [anySegment('a'), anyLine('l')],
	    assuming : [ (s, l) => s.isNontrivial && l.isNontrivial ],
	    with : (s, l) => [ s, l, s.heightTo(l).label('h') ],
	    hold :  (s, l, h) => h instanceof Segment && l .isContaining (h.end),
	    number : 200
	},
	{
	    name : "trigonometry",
	    for : [anyParam(0, 1/4)],
	    with : x => { let C = new Circle(10),
			      r = C.radius(x),
			      A = C.point(x),
			      s = new Segment('sin').at(A).heightTo(Ox),
			      c = new Segment('cos').at(A).heightTo(Oy),
			      t = new Segment('tan').at(A).tangentTo(C,1).extendTo(Ox),
			      ct = new Segment('cot').at(A).tangentTo(C,-1).extendTo(Oy),
			      phi = deg2rad(Angle.azimuth(origin, A))
			  return [phi, s, c, t, ct, r, C, Ox, Oy] },
	    hold :  (phi, s, c, t, ct, r) => (equal(s.length/r.length, sin(phi)) &&
					      equal(c.length/r.length, cos(phi)) &&
					      equal(t.length/r.length, tg(phi)) &&
					      equal(ct.length/r.length, ctg(phi))   ),
	    assuming : [x => x > 0.1 && x < 0.2]
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
	{name: 'IntersectionL 1',
	 for : [anyCircle(), anyLine()],
	 assuming : [(c,l) => c.isNontrivial && l.isNontrivial],
	 with : (c, l) => [ c, l, c.intersectionL(l), c.center.distance(l) ],
	 hold : (c, l, i, d) => (d >  c.R ? i.length == 0 :
				 d == c.R ? i.length == 1 :
				 i.length == 2)
	},
	{name: 'isomorphism 1',
	 for : [anyCircle(), anyParam()],
	 hold : (c, t) => equal(c.locus(c.point(t)), t),
	 assuming : [ c => c.isNontrivial ]
	},
	{name: 'isomorphism 2',
	 for : [anyCircle(), anyParam()],
	 with : (c, t) => [ c, c.point(t), t ],
	 hold : (c, p) => equal(c.point(c.locus(p)), p),
	 assuming : [ c => c.isNontrivial ]
	},
	{name : 'radius',
	 suite :[
	     {name: '1', run : () => new Circle(3).radius(0).end, result : point([3,0])	},
	     {name: '2', run : () => new Circle(3).radius(1/2).end, result : point([-3,0]) },
	     {name: '3', run : () => new Circle(3).radius(1/4).end, result : point([0,3]) },
	     {name: '4', run : () => new Circle(3).radius(1).end, result : point([3,0])	}]
	},
	{name : 'tangent',
	 suite :[
	     {name: '1', run : () => new Circle(3).tangent(0).vector, result : [0,1] },
	     {name: '2', run : () => new Circle(3).tangent(1/2).vector, result : [0,-1] },
	     {name: '3', run : () => new Circle(3).tangent(1/4).vector, result : [-1,0] },
	     {name: '4', run : () => new Circle(3).tangent(1).vector, result : [0,1] }]
	}	
    ]
}


var allTests = {
    number: 200,
    log: true,
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
console.log('Running tests...')
runTests(allTests)
console.log('Testing done')
