var testLineEquation = new testSuite
('LineEquation',
 [ new test("1", lineEquation([0,0],[1,2])(0).xy, [0,0])
   , new test("2", lineEquation([0,0],[1,2])(1).xy, [1,2])
   , new test("3", lineEquation([0,0],[1,2])(1).xy, [-1,2], nequal)
   , new test("4", lineEquation([0,0],[1,2],2)(1).xy, [2,4])
   , new test("5", lineEquation([0,0],[1,2],2)(-1).xy, [-2,-4])
   , new test("6", lineEquation([0,0],[0,1])(0).xy, [0,0])
   , new test("7", lineEquation([0,0],[0,1])(2).xy, [0,2])
   , new test("8", lineEquation([1,2],[1,2])(0).xy, [1,2])
   , new test("9", lineEquation([1,2],[1,2])(1).xy, [1,2])
   , new test("10", lineEquation([1,2],[0,0],-1)(0).xy, [1,2])
   , new test("11", lineEquation([1,2],[0,0],-1)(1).xy, [2,4])
 ]);

var testIntersectionV = new testSuite
('intersectionV',
 [ new test("1", intersectionV([0,0],[1,2],[0,0],[2,1]), [0,0])
   , new test("2", intersectionV([1,2],[1,1],[2,1],[-1,1]), [1,2])
   , new test("3", intersectionV([1,0],[0,1],[0,1],[1,0]), [1,1])
   , new test("5", intersectionV([1,2],[-1,-2],[3,4],[-3,-4]), [0,0])
   , new test("6", intersectionV([1,2],[1,2],[3,4],[2,4]), [1/0,1/0])
   , new test("7", intersectionV([1,2],[1,2],[1,2],[-2,-4]), [1/0,1/0])
 ]);

var pnt1 = new Point('A').at([1,2]);
var pnt2 = new Point().at([3,4]);
var lin1 = line(point([1,2]),point([4,6]));
var lin2 = line(point([-2,3]),point([4,-6]));
var ray1 = ray(point([1,2]),point([5,6]));
var seg1 = seg(point([1,2]),point([6,6]));
var pol1 = triangle(pnt1, pnt2, origin);
var cir1 = new Circle(3).at(point([1,2]));
var ang1 = new Angle(30).on(lin1);

var testTransformations = new testSuite
('Transformations',
 [ new test("shift 3", new Line().shift([1,2]).pivot, [1,2])
   , new test("shift 4", new Line().shift([1,2]).vector, [1,0])
   , new test("shift 5", new Circle(2).shift([1,2]).center.xy, [1,2])
   , new test("shift 6", new Circle(2).shift([1,2]).R, 2)
   , new test("shift 7", seg1.shift([5,6]).start.xy, [6,8])
   , new test("shift 8", seg1.shift([5,6]).end.xy, [11,12])
   , new test("shift 9", seg1.shift([5,6]).isParallelTo(seg1))
   , new test("shift 10", cir1.shift([5,6]).center.xy, [6,8])
   , new test("shift 11", cir1.shift([5,6]).R, 3)
 ]);

var grp1 = new Group([pnt1, pnt2, lin1, ray1, seg1, cir1, ang1]);
var grp2 = grp1.copy();

var testGroup = new testSuite
('Group',
 [ new test("identification", grp1.isCurve, false)
   , new test("elements 1", grp1.element(1).option('label'), "A")
   , new test("elements 2", grp1.element(6) instanceof Circle)
   , new test("elements 3", grp1.element(8).xy, [1,2])
   , new test("copy", range(1,8).every(i => grp2.element(i).isEqual(grp1.element(i))))
 ]);

var testPoints = new testSuite
('Point',
 [ new test("copy 0", pnt1.copy().label() == pnt1.label())
   , new test("copy 1", pnt2.copy().label("B") != pnt2.label())
   , new test("copy 2", pnt1.isEqual(pnt1.copy()))
   , new test("copy 3", pnt1.isEqual(origin.copy().shift(pnt1.xy)))
   , new test("identification", pnt1.isCurve, false)
   , new test("coordinates 1", new Point().xy, [0,0])
   , new test("coordinates 2", new Point().at([3,4]).xy, [3,4])
   , new test("equality 1", new Point().at([3,4]).isEqual(pnt2))
   , new test("equality 2", pnt1.isEqual(pnt1))
   , new test("equality 3", !pnt1.isEqual(new Point().at([3,4])))
   , new test("shift 1", new Point().shift([1,2]).xy, [1,2])
   , new test("shift 2", new Point().shift([1,2]).shift([-1,-2]), new Point())
   , new test("scale 1", pnt1.scale(2,3).xy, [2,6])
   , new test("scale 2", pnt1.scaleAt(pnt1,2,3).xy, pnt1.xy)
   , new test("superpose", pnt1.superpose(pnt1, pnt2).xy, pnt2.xy)
   , new test("rotate 1", pnt1.rotate(90).xy, [-2,1])
   , new test("rotate 2", pnt1.rotateAt(pnt1,90).xy, pnt1.xy)
   , new test("rotate 3", pnt1.rotateAt(pnt2,180).xy, [5, 6])
   , new test("reflect 1", pnt1.reflect(0).xy, [1, -2])
   , new test("reflect 2", pnt1.reflect(90).xy, [-1, 2])
   , new test("reflect 3", pnt1.reflect(45).xy, [2, 1])
   , new test("reflect 4", pnt1.reflectAt(point([2,3]), 135).xy, pnt2.xy)
   , new test("reflect 5", pnt1.reflectAt(pnt1, 45).xy, pnt1.xy)
   , new test("reflect 6", pnt1.reflectAt(point([2,3])).xy, [3,4])
   , new test("reflect 7", pnt2.reflectAt(lin1).xy, [2.36, 4.48])
   , new test("inside 1", pnt1.isInside)
   , new test("inside 2", !pnt1.shift([100,100]).isInside)
   , new test("on line 1", new Point().on(lin1,0).xy, lin1.pivot)
   , new test("on line 2", new Point().on(lin1,1).xy, lin1.pivot.vadd(lin1.vector))
   , new test("on line 3", new Point().on(seg1,0), seg1.start)
   , new test("on line 4", new Point().on(seg1,1), seg1.end)
   , new test("on circle 1", new Point().on(cir1,0).xy, [4,2])
   , new test("on circle 2", new Point().on(cir1,pi/2).xy, [1,5])
   , new test("on circle 3", new Point().on(cir1,4*pi).xy, [4,2])
   , new test("distance 1", pnt1.distance(pnt2), pnt2.distance(pnt1))
   , new test("distance 2", pnt1.distance(pnt1), 0)
   , new test("distance 3", pnt1.distance(pnt2), sqrt(8))
   , new test("distance 4", new Point().on(lin1,2).distance(lin1), 0)
   , new test("distance 5", new Point().on(cir1,2).distance(cir1), 0)
   , new test("distance 6", cir1.center.distance(cir1), cir1.R)
   , new test("between 1", new Point().between(pnt1,pnt2,0), pnt1)
   , new test("between 2", new Point().between(pnt1,pnt2,1), pnt2)
   , new test("between 3", new Point().between(pnt1,pnt2,1/2).xy, [2,3])
   , new test("azimuth 1", new Point().azimuth(pnt1,0,pnt2,40).xy, pnt2.xy)
   , new test("azimuth 2", new Point().azimuth(pnt1,40,pnt2,0).xy, pnt1.xy)
   , new test("azimuth 3", new Point().azimuth(pnt1,40,pnt2,-40).xy, [1/0,1/0])
   , new test("azimuth 4", new Point().azimuth(pnt1,0,pnt2,0).xy, [1/0,1/0])
   , new test("azimuth 5", angle(pnt2,pnt1,new Point().azimuth(pnt1,30,pnt2,45)).value, 30)
   , new test("azimuth 6", angle(new Point().azimuth(pnt1,30,pnt2,45),pnt2,pnt1).value, 45)
   , new test("azimuth 7", angle(pnt1,new Point().azimuth(pnt1,30,pnt2,90),pnt2).value, 60)
 ]);

var testLine = new testSuite
('Line',
 [ new test("identification", lin1.isCurve),
   testLineEquation
   , testIntersectionV
   , new test("copy 0", lin1.copy() instanceof Line)
   , new test("copy 1", lin1.copy().pivot, lin1.pivot)
   , new test("copy 2", lin1.copy().vector, lin1.vector)
   , new test("copy 3", lin1.copy().eqn(5), lin1.eqn(5))
   , new test("copy 4", lin1.copy().angle, lin1.angle)
   , new test("copy 5", lin1.copy().rotate(60), lin1.angle, nequal)
   , new test("shift 1", lin1.shift([4,5]).pivot, [5, 7])
   , new test("shift 2", lin1.shift([4,5]).angle, lin1.angle)
   , new test("scale 1", lin1.scale(4).angle, lin1.angle)
   , new test("scale 2", lin1.scale(4).pivot, [4,8])
   , new test("scale 3", lin1.scale(1,-1).angle, 360-lin1.angle)
   , new test("scale 4", lin1.scale(1,2).pivot, [1,4])
   , new test("superpose", lin1.superpose(pnt1,pnt2).pivot, [3,4])
   , new test("rotate 1", lin1.rotate(90).pivot, [-2,1])
   , new test("rotate 2", lin1.rotate(90).vector, [-4,3])
   , new test("rotate 3", lin1.rotateAt(point(lin1.pivot),35).pivot, lin1.pivot)
   , new test("reflect 1", lin1.reflect(90).pivot, [-1,2])
   , new test("reflect 2", lin1.reflect(90).vector, [-3,4])
   , new test("reflect 3", lin1.reflect(45).pivot, [2,1])
   , new test("reflect 4", lin1.reflect(45).vector, [4,3])
   , new test("reflect 5", lin1.reflectAt(lin1).pivot, [1,2])
   , new test("reflect 6", lin1.reflectAt(lin1).vector, [3,4])
   , new test("reflect 7", lin1.reflectAt(lin1).pivot, [1,2])
   , new test("reflect 8", lin1.reflectAt(ray1).vector, [4,3])
   , new test("reflect 9", lin1.reflectAt(origin).pivot, [-1,-2])
   , new test("reflect 10", lin1.reflectAt(origin).vector, [-3,-4])
   , new test("reflect 11", lin1.reflectAt(pnt2).pivot, [5,6])
   , new test("reflect 12", lin1.reflectAt(pnt2).vector, [-3,-4])
   , new test("locate 1", lin1.location(lin1.eqn(0)), 0)
   , new test("locate 2", lin1.location(lin1.eqn(2)), 2)
   , new test("locate 3", lin1.location(point(lin1.eqn(3).xy.vadd(lin1.norm))), 3)
   , new test("locate 4", lin1.location(point(lin1.eqn(-2).xy.vadd(lin1.norm))), -2)
   , new test("isContaining 1", lin1.isContaining(lin1.eqn(3)))
   , new test("isContaining 2", !lin1.isContaining(origin))
   , new test("isParallelTo 1", !lin1.isParallelTo(lin2))
   , new test("isParallelTo 2", lin1.isParallelTo(lin1))
   , new test("isParallelTo 3", lin1.isParallelTo(lin1.shift([10,20])))
   , new test("isParallelTo 4", lin1.isParallelTo(lin2.parallelTo(lin1)))
   , new test("isParallelTo 5", lin1.isParallelTo(lin2.atAngle(lin1.angle)))
   , new test("isParallelTo 6", lin1.isParallelTo(lin1.reflectAt(lin1.eqn(5))))
   , new test("isPerpendicularTo 1", !lin1.isPerpendicularTo(lin2))
   , new test("isPerpendicularTo 2", Oy.isPerpendicularTo(Ox))
   , new test("isPerpendicularTo 3", lin1.isPerpendicularTo(lin2.atAngle(lin1.angle - 90)))
   , new test("isEqual 1", lin1.isEqual(lin1.copy()))
   , new test("isEqual 2", !lin1.isEqual(lin2))
   , new test("isEqual 3", !lin1.isEqual(pnt1))
   , new test("isSimilar 1", lin1.isSimilar(line(lin1.eqn(8), lin1.eqn(2))))
   , new test("isSimilar 2", !lin1.isSimilar(line(lin2.eqn(0), lin2.eqn(1))))
   , new test("joining 1", new Line().joining(lin1.eqn(0),lin1.eqn(1)), lin1)
   , new test("joining 2", new Line().joining(lin1.eqn(0),lin1.eqn(0)).vector, [0,0])
   , new test("at 1", lin1.at(origin.xy).pivot,[0,0])
   , new test("atAngle 1", lin1.atAngle(60).angle,60)
   , new test("atAngle 2", lin1.atAngle(400).angle,40)
   , new test("through 1", lin1.through(pnt2).eqn(0), pnt2)
   , new test("through 2", lin1.through(pnt2).vector, lin1.vector)
   , new test("parallelTo 1", lin1.parallelTo(Ox).angle, 0)
   , new test("along 1", lin1.along(lin2).isParallelTo(lin2))
   , new test("perpendicularTo 1", lin1.perpendicularTo(lin2).isPerpendicularTo(lin2))
   , new test("bisectrisse 1", new Line().bisectrisse(ang1).angle, lin1.angle+15)
   , new test("midPerpendicular 1", new Line().midPerpendicular(seg1).isPerpendicularTo(seg1))
   , new test("midPerpendicular 2", new Line().midPerpendicular(seg1).eqn(0).xy, seg1.middle.xy)
   , new test("tangentTo 1"
	      , new Line().through(cir1.eqn(1)).tangentTo(cir1)
	      .isPerpendicularTo(ray(cir1.center, cir1.eqn(1))))
   , new test("tangentTo 2"
	      , new Line().through(point([5,0])).tangentTo(cir1)
	      .intersections(cir1).length, 1)
   , new test("tangentTo 3"
	      , new Line().through(point([5,0])).tangentTo(cir1,1)
	      .reflectAt(line(cir1.center, point([5,0])))
	      .isSimilar(new Line().through(point([5,0])).tangentTo(cir1,-1)))
   , new test("distance 1", lin1.distance(lin1.eqn(4)), 0)
   , new test("distance 2", new Segment().through(lin1.eqn(0)).perpendicularTo(lin1)
	      .extendToLength(10).end.distance(lin1), 10)
   , new test("distance 3", new Line().through(point([5,0])).tangentTo(cir1)
	      .distance(cir1), 0)
   , new test("distance 4", lin1.distance(cir1), 0, nequal)
   , new test("intersection 1", lin1.intersections(Ox)[0].xy, [-1/2,0])
   , new test("intersection 2", Oy.intersections(Ox)[0].xy, [0,0])
   , new test("intersection 3", lin1.intersections(lin1)[0].xy, [1/0,1/0])
   , new test("intersection 4", new Circle().intersections(Ox).map(p => p.xy), [[-1,0],[1,0]])
   , new test("intersection 5", new Circle().shift([0,1]).intersections(Ox).map(p => p.xy), [[0,0]])
   , new test("intersection 6", new Circle().shift([0,1.1]).intersections(Ox), [])
 ]);

var testRay = new testSuite
('Ray',
 [ new test("identification", ray1.isCurve),
   , new test("copy 1", ray1.copy().pivot, ray1.pivot)
   , new test("copy 2", ray1.copy().vector, ray1.vector)
   , new test("copy 3", ray1.copy().eqn(5), ray1.eqn(5))
   , new test("copy 4", ray1.copy().angle, ray1.angle)
   , new test("isContaining 1", ray1.isContaining(ray1.start))
   , new test("isContaining 2", ray1.isContaining(ray1.eqn(2)))
   , new test("isContaining 3", !ray1.isContaining(ray1.eqn(-2)))
   , new test("isContaining 4", !ray1.isContaining(origin))
   , new test("isEqual 1", ray1.isEqual(ray1))
   , new test("isEqual 2", !ray1.isEqual(new Ray()))
   , new test("isEqual 3", !ray1.isEqual(new Ray().through(ray1.start).parallelTo(ray1)))
   , new test("isEqual 4", !ray1.isEqual(ray1.flip()))
   , new test("isSimilar 1", ray1.isSimilar(new Ray().through(ray1.start).parallelTo(ray1)))
   , new test("isSimilar 2", !ray1.isSimilar(new Ray().through(ray1.eqn(1)).parallelTo(ray1)))
   , new test("isSimilar 3", !ray1.isSimilar(ray1.flip()))
   , new test("flip 1", ray1.isEqual(ray1.flip().flip()))
   , new test("flip 2", ray1.start.isEqual(ray1.flip().start))
   , new test("flip 3", equal(ray1.unit, ray1.flip().unit.scale(-1)))
 ]);

var testSegment = new testSuite
('Segment',
 [ new test("identification", seg1.isCurve),
   new test("copy 1", seg1.copy().start.xy, seg1.start.xy)
   , new test("copy 2", seg1.copy().end.xy, seg1.end.xy)
   , new test("copy 3", seg1.copy().isEqual(seg1))
   , new test("copy 4", !seg1.copy().isEqual(seg1.rotate(30)))
   , new test("isContaining 1", seg1.isContaining(seg1.eqn(0)))
   , new test("isContaining 2", seg1.isContaining(seg1.eqn(1/2)))
   , new test("isContaining 3", !seg1.isContaining(seg1.eqn(1.4)))
   , new test("isContaining 4", !seg1.isContaining(seg1.eqn(-1.4)))
   , new test("isContaining 4", !seg1.isContaining(origin))
   , new test("isSimilar 4", seg1.isSimilar(seg1.rotate(30)))
   , new test("isSimilar 4", !seg1.isSimilar(seg1.scale(2)))
   , new test("isSimilar 4", seg1.isSimilar(seg1.copy()))
   , new test("extend 1", seg1.extend(0,1).isEqual(seg1))
   , new test("extend 2", seg1.extend(0,2).length, seg1.length*2)
   , new test("extend 3", seg1.extend(-2,0).length, seg1.length*2)
   , new test("extend 4", seg1.extend(0,2).isContaining(seg1.eqn(1.4)))
   , new test("extend 5", seg1.extend(1).isEqual(seg1))
   , new test("extend 6", seg1.extend(2).isEqual(seg1.extend(0,2)))
   , new test("extendToLength 1", seg1.extendToLength(seg1.length).isEqual(seg1))
   , new test("extendToLength 2", seg1.extendToLength(-1).length, 1)
   , new test("extendToLength 3", seg1.extendToLength(25).length, 25)
   , new test("extendToLine 1", seg1.extendToLine(Ox).start.isEqual(seg1.start))
   , new test("extendToLine 2", Ox.isContaining(seg1.extendToLine(Ox).end))
   , new test("extendToLine 3", seg1.extendToLine(Ox).isParallelTo(seg1))
   , new test("extendToLine 4"
	      , seg1.extendToLine(new Ray().parallelTo(seg1))
	      .isSimilar(new Ray().through(seg1.start).parallelTo(seg1)))
   , new test("heightTo 1", seg1.heightTo(Ox).length, seg1.start.y)
   , new test("heightTo 2", seg1.heightTo(Oy).length, seg1.start.x)
   , new test("heightTo 3", seg1.heightTo(lin1).length, 0)
   , new test("heightTo 4", seg1.heightTo(lin2).isPerpendicularTo(lin2))
   , new test("tangentTo 1"
	      , new Segment().through(cir1.eqn(1)).tangentTo(cir1)
	      .isPerpendicularTo(ray(cir1.center, cir1.eqn(1))))
   , new test("tangentTo 2"
	      , new Segment().through(point([5,0])).tangentTo(cir1)
	      .intersections(cir1).length, 1)
   , new test("tangentTo 3"
	      , new Segment().through(point([5,0])).tangentTo(cir1,1)
	      .reflectAt(line(cir1.center, point([5,0])))
	      .isSimilar(new Segment().through(point([5,0])).tangentTo(cir1,-1)))

 ]);

var testAngle = new testSuite
('Angle',
 [ new testSuite
   ('angleV',
    [ new test("angleV 1", angleV([0,0]), 0)
      , new test("angleV 2", angleV([1,1]), 45)
      , new test("angleV 3", angleV([-1,1]), 135)
      , new test("angleV 4", angleV([-1,-1]), 225)
      , new test("angleV 5", angleV([1,-1]), 315)
    ])
   , new test("identification", !ang1.isCurve),
   , new test("copy 1", ang1.copy().isEqual(ang1))
   , new test("copy 2", ang1.value, ang1.copy().on(seg1), nequal)
   , new test("isEqual 1", ang1.isEqual(ang1))
   , new test("isEqual 2", !ang1.isEqual(new Angle(45)))
   , new test("isEqual 3", ang1.isEqual(ang1.copy()))
   , new test("isSimilar 1", ang1.isSimilar(new Angle(30)))
   , new test("isSimilar 2", ang1.isSimilar(new Angle(360+30)))
   , new test("isSimilar 3", !ang1.isSimilar(new Angle(60)))
   , new test("on 1", new Angle(10).on(seg1).start, seg1.angle)
   , new test("on 2", new Angle(10).on(seg1).end, seg1.angle+10)
   , new test("on 3", new Angle(10).on(seg1).vertex, seg1.start)
   , new test("on 4", new Angle(10).on(seg1).value, 10)
   , new test("at 1", new Angle().at(pnt1).vertex.xy, pnt1.xy)
   , new test("shift")
   , new test("scale")
   , new test("superpose")
   , new test("rotate")
   , new test("reflect")
 ]);

var testPolygon = new testSuite
('Polygon',
 [ new test("identification", pol1.isCurve),
   new test("copy 1", seg1.copy().start.xy, seg1.start.xy)
   , new test("copy 2", seg1.copy().end.xy, seg1.end.xy)
   , new test("shift")
   , new test("scale")
   , new test("superpose")
   , new test("rotate")
   , new test("reflect")
 ]);

var testCircle = new testSuite
('Circle',
 [ new test("identification", cir1.isCurve),
   new test("copy 1", seg1.copy().start.xy, seg1.start.xy)
   , new test("copy 2", seg1.copy().end.xy, seg1.end.xy)
   , new test("shift")
   , new test("scale")
   , new test("superpose")
   , new test("rotate")
   , new test("reflect")
 ]);


var allTests = new testSuite
('all', [ testPoints
	  , testLine
	  , testRay
	  , testSegment
	  , testPolygon
	  , testCircle
	  , testAngle
	  , testTransformations
	  , testGroup
	]);

allTests.run();
