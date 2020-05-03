labeled = (l) -> (f) -> (...x) -> if l then f(...x).label(l) else f(...x)

anyParam = (a = 0, b = 1) ->
  anyNum()
  .range(a, b)
  .precision(0.01)
  .ascending()

anyXY = -> ArbitraryCoordinates()

anyPoint = (l) ->
  args(anyXY())
  .iso(labeled(l)(point), (p) -> [p.xy])

anyAngle = -> anyNum().range(0,360).precision(1)

anyLine = (l) ->
  args(anyPoint(), anyPoint())
  .iso(labeled(l)(line), (l) -> [l.point(0), l.point(1)])

anySegment = (l) ->
  args(anyPoint(), anyPoint())
  .iso(labeled(l)(segment), (l) -> [l.point(0), l.point(1)])

anyRay = (l) ->
  args(anyPoint(), anyAngle())
  .iso(labeled(l)(ray), (r) -> [r.start, r.angle(0)])

anyRadius = ->
  anyNum()
  .range(1, paperSize/4)
  .precision(1)
  .ascending()

anyCircle = ->
  args(anyPoint(), anyRadius())
  .iso(circle, (c) -> [c.center, c.R])
  .ascending()

triangle = (a,b,c) -> new Triangle  a, b, c

anyTriangle = ->
  args(anyPoint(), anyPoint(), anyPoint())
  .iso(triangle, (c) -> c.vertices)
  .filter((t) -> t.isNontrivial)

############################################################

testTests =
  name: \Tests
  skip: yes
  log: on
  suite: 
    * name: "1"
      for:
        anyNum().range(0,100).precision(0.1)
      hold: (n) -> n == 4
      number: 10
      
    * name: "2"
      for:
        anyNum()
        anyNum()
      hold: (n, m) -> n >= m
      log: off
      
    * name: "3"
      for:
        anyCircle()
        anyPoint()
      assuming:
        (c, p) -> c.R > 1 and (c.center .isNotEqual p)
      hold: (c, p) -> ! (c .isEnclosing  p)

    * name: "tangentTo 2"
      for:
        anyCircle()
        anyParam()
      with: (c, x) -> [ c, x,
                        new Line().at(c.point(x)).tangentTo(c),
                        c.radius(x)]
      hold: (c, x, t) -> t.isPerpendicularTo(c.radius(x))
      assuming:
        (c) -> c.isNontrivial

    * name: \trigonometry
      for:
        anyParam(0, 1/4)
      with: (x) ->
        C = new Circle(10)
        r = C.radius(x)
        A = C.point(x)
        s = new Segment('sin').at(A).heightTo(Ox)
        c = new Segment('cos').at(A).heightTo(Oy)
        t = new Segment('tan').at(A).tangentTo(C,1).extendTo(Ox)
        ct = new Segment('cot').at(A).tangentTo(C,-1).extendTo(Oy)
        phi = deg2rad(Angle.azimuth(origin, A))
        [phi, s, c, t, ct, r, C, Ox, Oy]
      hold:  (phi, s, c, t, ct, r) ->
         equal  s.length/r.length,  sin phi and
         equal  c.length/r.length,  cos phi and
         equal  t.length/r.length,  tg  phi and
         equal  ct.length/r.length, ctg phi
      assuming : [(x) -> x > 0.1 && x < 0.2]

############################################################

testPoints = 
  name: \Points
  number: 10
  suite:
    * name: "isomorphism 1"
      for: [ anyPoint() ]
      holds: (p) -> Point.iso(Point.iso(p).xy).point .isEqual (p)
      
    * name: "copy 1"
      run: -> point([3,4]).copy()
      result: point([3,4])
      
    * name: "superpose"
      for: [anyPoint(), anyPoint()]
      holds: (p1,p2) -> p1.superpose(p1,p2) .isEqual (p2)
      
    * name: "isomorphism 1"
      for: [anyPoint()],
      hold: (p) -> Point.iso(Point.iso(p).xy).point .isEqual (p)
      
    * name: "isomorphism 2"
      for: [anyXY()],
      hold: (xy) -> equal(Point.iso(Point.iso(xy).point).xy, xy)
      
    * name: "copy 1"
      for: [anyPoint()]
      hold: (p) -> p.copy() .isEqual (p)
      
    * name: "coordinates 1"
      for: [anyPoint()],
      hold: (p) -> origin.at(p) .isEqual (point(p))
      
    * name: "equality 1"
      for: [anyPoint()]
      hold: (p) -> p .isEqual (p)
      
    * name: "equality 2"
      for: [anyPoint()]
      hold: (p) -> point(p.xy) .isEqual (p)
      
    * name: "equality 3"
      for: [anyPoint()],  
      hold: (p) -> !p.copy().translate([1,1]) .isEqual (p)
      
    * name: "translate 1" 
      run: -> new Point().translate([1,2]).xy
      result: [1,2]

############################################################

testTransformations = 
  name: \Transformations
  suite: [ ]

############################################################

testLines =
  name: 'Lines'
  suite: 
    number: 10
    suite: 
      * name: "isomorphism 1"
        for:
          anyLine()
          anyNum()
        assuming: [ (l) -> l.isNontrivial ]
        hold: (l, s) -> equal l.locus(l.point(s)), s

      * name: "isomorphism 2"
        for:
          anyPoint()
          anyNum()
        where: (p, s) -> [ l, s ]
        hold: (p, s) ->
          l = line p, p
          l.isTrivial and equal l.locus(l.point(s)), 0
    ...

    #     * name: "isomorphism 3",
    # 		  for: [anyLine(), anyNum()]
    # 		  assuming: [(l) -> l.isNontrivial]
    # 		  hold: (l, s) ->
    #         p = l.point(s)
    #         equal l.point(l.locus(p)), p

    #     * name: "equation 1"
    # 		  for: [anyLine(), anyNum()]
    # 		  hold: (l, s) -> l.equation(l.point(s))

    #     * name: "equation 2"
    # 		  for: [anyLine(), anyNum()]
    # 		  assuming: [(l) -> l.isNontrivial]
    # 		  hold: (l, s) -> !l.equation(l.point(s).translate(l.normalV(0)))

    # * name: "line intersections 1"
    #   skip: true
    #   for: [anyLine(), anyLine()]
    #   assuming: [ (a,b) -> !(a.isTrivial || b.isTrivial) ]
    #   hold: (a,b) ->
    #     ps = a.intersections(b)
    #     ps.length = if a .isParallelTo b then 0 else 1

    # * name: "plane intersections 1"
	  #   for: [anyLine()]
	  #   assuming: [ (l) -> plane.isEnclosing(l.refPoint) ]
	  #   hold: (l) ->
    #     ps = plane.intersections(l)
		# 	  ps.every((p) -> plane.isContaining(p)) }

    # * name: "perpendicularity 1"
	  #   for: [anyLine('a'), anyPoint('A')]
	  #   with: (l, p) -> [l, p, new Line('b').at(p).perpendicularTo(l) ]
	  #   hold: (l, p, nl) -> nl.isPerpendicularTo(l)
	  #   assuming: [(l) -> l.isNontrivial]
	
	  # * name: "perpendicularity 2"
	  #   for: [anyLine()]
	  #   hold: (l) -> !l.isPerpendicularTo(line(origin, origin))
	
	  # * name: "perpendicularity 3"
	  #   for: [anyLine('1'), anyLine('2')]
	  #   with: (l1, l2) -> [ l1, l2, l1.perpendicularTo(l2).label('p') ]
	  #   hold: (l1, l2, p) ->
    #     (p.refPoint .isEqual (l1.refPoint) and
    #     gequal(p.locus(p.intersections(l2)[0]), 0)),
	  #   assuming: [ (l1, l2, p) -> l1.isNontrivial and l2.isNontrivial ]
	
	  # * name: "parallelity 1"
	  #   for: [anyLine(), anyXY()]
	  #   assuming: [(l) -> l.isNontrivial]
	  #   hold: (l, xy) -> new Line().at(xy).parallelTo(l).isParallelTo(l)
	
	  # * name: "parallelity 2"
	  #   for: [anyLine()]
	  #   hold: (l) -> !l.isParallelTo(line(origin, origin))
	
	  # * name: "tangentTo 1"
	  #   for: [anyPoint(), anyCircle()]
	  #   with: (p, c) ->
    #     t = new Line().at(p).tangentTo(c)
    #     i = c.intersections(t)
    #     [p, c, t, i]},
	  #   hold: (p, c, t, i) ->
    #     i.length == 1 and
    #     new Line().at(i[0]).perpendicularTo(t).isContaining(c.center))
	  #   assuming: [ (p, c) -> c.isNontrivial && !c.isEnclosing(p) ]
	
	  # * name: "tangentTo 2"
	  #   for: [anyCircle(), anyParam()]
	  #   with: (c, x) -> [c, x, new Line().at(c.point(x)).tangentTo(c), c.radius(x)]
	  #   hold: (c, x, t) -> t.isPerpendicularTo(c.radius(x))
	  #   assuming: [ (c) -> c.isNontrivial ]

############################################################

allTests =
  number: 50
  log: true
  suite: 
    testTests
    testPoints
    testLines

console.log 'Running tests...'
runTests allTests
console.log 'Testing done'
