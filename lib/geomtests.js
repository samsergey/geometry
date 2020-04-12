var testLineEquation = new testSuite
('LineEquation',
 [ new test("1", lineEquation([0,0],[1,2])(0), [0,0])
   , new test("2", lineEquation([0,0],[1,2])(1), [1,2])
   , new test("3", lineEquation([0,0],[1,2])(1), [-1,2], nequal)
   , new test("4", lineEquation([0,0],[1,2],2)(1), [2,4])
   , new test("5", lineEquation([0,0],[1,2],2)(-1), [-2,-4])
   , new test("6", lineEquation([0,0],[0,1])(0), [0,0])
   , new test("7", lineEquation([0,0],[0,1])(2), [0,2])
   , new test("8", lineEquation([1,2],[1,2])(0), [1,2])
   , new test("9", lineEquation([1,2],[1,2])(1), [1,2])
   , new test("10", lineEquation([1,2],[0,0],-1)(0), [1,2])
   , new test("11", lineEquation([1,2],[0,0],-1)(1), [2,4])
 ])

var testIntersectionV = new testSuite
('intersectionV',
 [ new test("1", intersectionV([0,0],[1,2],[0,0],[2,1]), [0,0])
   , new test("2", intersectionV([1,2],[1,1],[2,1],[-1,1]), [1,2])
   , new test("3", intersectionV([1,0],[0,1],[0,1],[1,0]), [1,1])
   , new test("5", intersectionV([1,2],[-1,-2],[3,4],[-3,-4]), [0,0])
   , new test("6", intersectionV([1,2],[1,2],[3,4],[2,4]), [1/0,1/0])
   , new test("7", intersectionV([1,2],[1,2],[1,2],[-2,-4]), [1/0,1/0])
 ])

var pnt1 = new Point('A').at([1,2])
var lin1 = line(point([1,2]),point([4,6]))
var ray1 = ray(point([1,2]),point([4,6]))
var seg1 = seg(point([1,2]),point([4,6]))
var cir1 = new Circle(3).at(point([1,2]))
var ang1 = new Angle(30).on(seg1)

var testTransformations = new testSuite
('Transformations',
 [ new test("shift 3", new Line().shift([1,2]).pivot, [1,2])
   , new test("shift 4", new Line().shift([1,2]).vector, [1,0])
   , new test("shift 5", new Circle(2).shift([1,2]).center.pt, [1,2])
   , new test("shift 6", new Circle(2).shift([1,2]).radius, 2)
   , new test("shift 7", seg1.copy().shift([5,6]).start.pt, [6,8])
   , new test("shift 8", seg1.copy().shift([5,6]).end.pt, [9,12])
   , new test("shift 9", seg1.copy().shift([5,6]).isParallelTo(seg1), true)
   , new test("shift 10", cir1.copy().shift([5,6]).center.pt, [6,8])
   , new test("shift 11", cir1.copy().shift([5,6]).radius, 3)
 ])


var pnt2 = new Point().at([3,4])

var testPoints = new testSuite
('Point',
 [ new test("coordinates 1", new Point().pt, [0,0])
   , new test("coordinates 2", new Point().at([3,4]).pt, [3,4])
   , new test("equality 1", new Point().at([3,4]).isEqual(pnt2),true)
   , new test("equality 2", pnt1.isEqual(pnt1), true)
   , new test("equality 3", pnt1.isEqual(new Point().at([3,4])), false)
   , new test("copy 1", pnt1.isEqual(pnt1.copy()), true)
   , new test("copy 2", pnt1.isEqual(origin.copy().shift(pnt1.pt)),true)
   , new test("shift 1", new Point().shift([1,2]).pt, [1,2])
   , new test("shift 2", new Point().shift([1,2]).shift([-1,-2]), new Point())
   , new test("scale 1", pnt1.copy().scale(2,3).pt, [2,6])
   , new test("scale 2", pnt1.copy().scaleAt(pnt1,2,3).pt, pnt1.pt)
   , new test("superpose", pnt1.copy().superpose(pnt1, pnt2).pt, pnt2.pt)
   , new test("rotate 1", pnt1.copy().rotate(90).pt, [-2,1])
   , new test("rotate 2", pnt1.copy().rotateAt(pnt1,90).pt, pnt1.pt)
   , new test("rotate 3", pnt1.copy().rotateAt(pnt2,180).pt, [5, 6])
   , new test("reflect 1", pnt1.copy().reflect(0).pt, [1, -2])
   , new test("reflect 2", pnt1.copy().reflect(90).pt, [-1, 2])
   , new test("reflect 3", pnt1.copy().reflect(45).pt, [2, 1])
   , new test("reflect 4", pnt1.copy().reflectAt(point([2,3]), 135).pt, pnt2.pt)
   , new test("reflect 5", pnt1.copy().reflectAt(pnt1, 45).pt, pnt1.pt)
   , new test("reflect 6", pnt1.copy().reflectAt(point([2,3]), 45).pt, pnt1.pt)
   , new test("reflect 7", pnt2.copy().reflectAt(lin1).pt, [2.36, 4.48])
 ])

var testLine = new testSuite
('Line',
 [ testLineEquation
   , testIntersectionV
   , new test("copy 1", lin1.copy().pivot, lin1.pivot)
   , new test("copy 2", lin1.copy().vector, lin1.vector)
   , new test("copy 3", lin1.copy().eqn(5), lin1.eqn(5))
   , new test("copy 4", lin1.copy().angle, lin1.angle)
 ])

var testRay = new testSuite
('Ray',
 [ new test("copy 1", ray1.copy().pivot, ray1.pivot)
   , new test("copy 2", ray1.copy().vector, ray1.vector)
   , new test("copy 3", ray1.copy().eqn(5), ray1.eqn(5))
   , new test("copy 4", ray1.copy().angle, ray1.angle)
 ])

var testSegment = new testSuite
('Segment',
 [ new test("copy 1", seg1.copy().start.pt, seg1.start.pt)
   , new test("copy 2", seg1.copy().end.pt, seg1.end.pt)
 ])

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
   , new test("constuction 1", new Angle(10).on(seg1).start, seg1.angle)
   , new test("constuction 2", new Angle(10).on(seg1).end, seg1.angle+10)
   , new test("constuction 3", new Angle(10).on(seg1).vertex, seg1.start)
 ])

var allTests = new testSuite
('all', [ testTransformations
	  , testPoints
	  , testLine
	  , testRay
	  , testSegment
	  , testAngle
	])

allTests.run()
