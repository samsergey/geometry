var testAngles = new testSuite
('Angle computation',
 [
     new test("angleV 1", angleV([0,0]), 0),
     new test("angleV 2", angleV([1,1]), 45),
     new test("angleV 3", angleV([-1,1]), 135),
     new test("angleV 4", angleV([-1,-1]), 225),
     new test("angleV 5", angleV([1,-1]), 315)
 ])

var testPoints = new testSuite
('Point',
 [
     new test("coordinates 1", new Point().pt, [0,0]),
     new test("coordinates 2", new Point().at([3,4]).pt, [3,4])
 ])

var allTests = new testSuite
('all', [
    testAngles,
    testPoints
])
