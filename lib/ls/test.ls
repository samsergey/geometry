implies = (P, Q) -> if P then Q else true

labeled = (l) -> (f) -> (...x) -> if l then f(...x).label(l) else f(...x)

any-param = (a = 0, b = 1) ->
    any-num!
    .range(a, b)
    .precision(0.01)
    .ascending!

any-XY = -> ArbitraryCoordinates!

any-point = (l) ->
  args(any-XY!)
  .iso(labeled(l)(point), (p) -> [p.xy])

any-angle-value = -> any-num!.range(0,360).precision(1)

any-angle = ->
  any-angle-value!
  .iso(((a) -> new Angle a), (a) -> a.value)

any-line = (l) ->
  args(any-point!, any-point!)
  .iso(labeled(l)(line), (l) -> [l.point(0), l.point(1)])

any-segment = (l) ->
  args(any-point!, any-point!)
  .iso(labeled(l)(segment), (l) -> [l.point(0), l.point(1)])

any-ray = (l) ->
  args(any-point!, any-angle-value!)
  .iso(labeled(l)(ray), (r) -> [r.start, r.angle(0)])

any-radius = ->
  any-num!
  .range(1, paper-size/4)
  .precision(1)
  .ascending!

any-circle = ->
  args(any-point!, any-radius!)
  .iso(circle, (c) -> [c.center, c.R])
  .ascending!

any-triangle = ->
  args(any-point!, any-point!, any-point!)
  .iso(new Triangle, (c) -> c.vertices)
  .filter((t) -> t.isNontrivial)

any =
  num: any-num
  param: any-param
  point: any-point
  XY: any-XY
  angle: any-angle
  angle-value: any-angle-value
  line: any-line
  ray: any-ray
  segment: any-segment
  circle: any-circle
  triangle: any-triangle
  
window <<< {any}

############################################################

test-tests =
  name: \Tests
  skip: yes
  log: on
  suite: 
    * name: "1"
      for: [ any-num!.range(0,100).precision(0.1) ]
      hold: (n) -> n == 4
      number: 10
      
    * name: "2"
      for:
        any-num!
        any-num!
      hold: (n, m) -> n >= m
      log: off
      
    * name: "3"
      for:
        any-circle!
        any-point!
      assuming:
        (c, p) -> c.R > 1 and (c.center .is-Not-equal p)
      hold: (c, p) -> ! (c .is-enclosing  p)

############################################################

testPoints = 
  name: \Points
  number: 10
  suite:
    * name: "isomorphism 1"
      for: [ any-point(\A) ]
      holds: (p) -> Point.iso(Point.iso(p).xy).point .is-equal p
      
    * name: "copy 1"
      run: -> point [3 4] .copy!
      result: point [3,4]
      
    * name: "superpose"
      for: [ any-point!, any-point! ]
      holds: (p1,p2) -> p1.superpose(p1,p2) .is-equal p2
      
    * name: "isomorphism 1"
      for: [ any-point! ],
      hold: (p) -> Point.iso(Point.iso(p).xy).point .is-equal p
      
    * name: "isomorphism 2"
      for: [ any-XY! ],
      hold: (xy) -> Point.iso(Point.iso(xy).point).xy `equal` xy
      
    * name: "copy 1"
      for: [ any-point! ]
      hold: (p) -> p.copy! .is-equal p
      
    * name: "coordinates 1"
      for: [ any-point! ]
      hold: (p) -> origin .at p .is-equal (point p)
      
    * name: "equality 1"
      for: [ any-point! ]
      hold: (p) -> p .is-equal p
      
    * name: "equality 2"
      for: [ any-point! ]
      hold: (p) -> point(p.xy) .is-equal p
      
    * name: "equality 3"
      for: [ any-point! ],  
      hold: (p) -> ! p .copy! .translate [1 1] .is-equal p
      
    * name: "translate 1" 
      run: -> new Point! .translate [1 2] .xy
      result: [1,2]


############################################################

test-transformations = 
  name: \Transformations
  suite: [ ]

############################################################

test-lines =
  name: 'Lines'
  suite:
    number: 10
    suite: 
      * name: "isomorphism 1"
        for:
          any-line!
          any-num!
        assuming: [ (l) -> l .is-nontrivial ]
        hold: (l, s) -> (l.locus l.point s) `equal` s

      * name: "isomorphism 2"
        for:
          any-point!
          any-num!
        where: (p, s) -> [ l, s ]
        hold: (p, s) ->
          l = line p, p
          l .is-trivial and (l.locus l.point s) `equal` 0

      * name: "isomorphism 3",
        for: [any-line!, any-num!]
        assuming: [(l) -> l.is-nontrivial]
        hold: (l, s) ->
          p = l.point s
          (l.point l.locus p) `equal` p

      * name: "equation 1"
        for: [ any-line!, any-num! ]
        hold: (l, s) -> l.equation l.point s

      * name: "equation 2"
        for: [any-line!, any-num!]
        assuming: [ (l) -> l.is-nontrivial ]
        hold: (l, s) -> ! l.equation l.point(s).translate l.normalV 0
    ...

    * name: "line intersections 1"
      for: [any-line!, any-line!]
      assuming: [ (a, b) -> !(a .is-trivial or b .is-trivial) ]
      hold: (a, b) ->
        (a .intersections b).length == if (a .is-parallel-to b) then 0 else 1

    * name: "plane intersections 1"
      for: [any-line!]
      assuming: [ (l) -> plane .is-enclosing l.ref-point ]
      hold: (l) ->
        pts = plane .intersections l
        pts.every((p) -> plane .is-containing p)

    * name: "perpendicularity 1"
      for:
        any-line \a
        any-point \A
      with: (l, p) -> [l, p, new Line \b .at p .perpendicular-to l ]
      assuming: [(l) -> l.is-nontrivial]
      hold: (l, p, nl) -> nl .is-perpendicular-to l
	
    * name: "perpendicularity 2"
      for: [any-line!]
      hold: (l) -> ! l .is-perpendicular-to (line origin, origin)
	
    * name: "perpendicularity 3"
      for:
        any-line \1
        any-line \2
      with: (l1, l2) -> [ l1, l2, l1 .perpendicular-to l2 .label \p ]
      assuming: [ (l1, l2, p) -> l1.is-nontrivial and l2.is-nontrivial ]
      hold: (l1, l2, p) ->
        p.ref-point .is-equal l1.ref-point and
        (p.locus (p .intersections l2).0) `gequal` 0

    * name: "parallelity 1"
      for: [any-line!, any-XY!]
      assuming: [(l) -> l.is-nontrivial]
      hold: (l, xy) -> new Line! .at xy .parallel-to l .is-parallel-to l
	
    * name: "parallelity 2"
      for: [any-line!]
      hold: (l) -> ! l .is-parallel-to (line origin, origin)
	
    * name: "tangentTo 1"
      for: [any-point!, any-circle!]
      with: (p, c) ->
        t = new Line! .at p .tangentTo c
        i = c .intersections t
        [p, c, t, i]
      hold: (p, c, t, i) ->
        i.length == 1 and
        new Line! .at (i.0) .perpendicular-to t .is-containing c.center
      assuming: [ (p, c) -> c.is-nontrivial and ! c .is-enclosing p ]
	
    * name: "tangentTo 2"
      for: [any-circle!, any-param!]
      with: (c, x) -> [ c, x, new Line! .at (c.point x) .tangentTo c ]
      hold: (c, x, t) -> t .is-perpendicular-to (c.radius x)
      assuming: [ (c) -> c.is-nontrivial ]

    * name: 'tangentTo 3'
      for: [any-point!, any-circle!],
      with: (p, c) ->
        t1 = new Line! .at p .tangent-to c, 1
        t2 = new Line! .at p .tangent-to c, -1
        i1 = c .locus (c .intersections t1).0
        i2 = c .locus (c .intersections t2).0
        [p, c, t1, t2, i1, i2]
      hold: (p, c, t1, t2, i1, i2) ->
        (t1.vector.dot(c.tangent-v(i1)) > 0) and
        (t2.vector.dot(c.tangent-v(i2)) < 0)
      assuming: [(p, c) -> c.is-nontrivial and ! c .is-enclosing p]

    * name: \LineEquation
      number: 10
      suite: 
        * name: \1
          for: [any-XY!, any-XY!]
          hold: (p1,p2) -> line-equation(p1, p2) 0 .is-equal (point p1)

        * name: \2,
          for: [any-XY(), any-XY()]
          hold: (p1,p2) -> line-equation(p1,p2) 1 .is-equal (point p2)

        * name: \4
          run: -> line-equation([0 0], [1 2], 2) 1
          result: point [2 4]
        * name: \5
          run: -> line-equation([0 0], [1 2], 2) -1
          result: point [-2 -4]
        * name: \6
          run: -> line-equation([0 0], [0 1]) 0
          result: point [0 0]
        * name: \7
          run: -> line-equation([0 0], [0,1]) 2
          result: point [0 2]
        * name: \8
          run: -> line-equation([1 2], [1 2]) 0
          result: point [1 2]
        * name: \9
          run: -> line-equation([1 2], [1 2]) 1
          result: point [1 2]
        * name: \10
          run: -> line-equation([1 2], [0 0],-1) 0
          result: point [1 2]
        * name: \11
          run: -> line-equation([1 2], [0 0],-1) 1
          result: point [2 4]

    * name:"intersectionV"
      suit:
        * name: \1
          run : -> intersection-v([0 0], [1 2], [0 0], [2 1])
          result : [0 0]
        * name: \2
          run : -> intersection-v([1 2], [1 1], [2 1], [-1 1])
          result : [1 2]
        * name: \3
          run : -> intersection-v([1 0], [0 1], [0 1], [1 0])
          result : [1 1]
        * name: \5
          run : -> intersection-v([1 2], [-1 -2], [3 4], [-3 -4])
          result : [0 0]
        * name: \6
          run : -> intersection-v([1 2], [1 2], [3 4], [2 4])
          result : [Infinity, Infinity]
        * name: \7
          run : -> intersection-v([1 2], [1 2], [1 2], [-2 -4])
          result : [Infinity, Infinity]
      
############################################################

test-rays = 
  name: \Rays
  suite: 
    * name: "is-containing 1",
      for: [ any-ray!, any-num! ]
      assuming: [ (r) -> r.is-nontrivial ]
      hold: (r, s) -> s >= 0 and r .is-containing (r.point s)

    * name: "is-containing 2"
      for: [ any-ray!, any-num! ]
      assuming: [ (r) -> r.is-nontrivial ]
      hold: (r, s) -> ! r .is-containing (r.point s .translate r.normalV 0)

    * name: "tangent-to"
      for: [any-circle!, any-param!]
      with: (c, x) ->
        p = c.point x
        t1 = new Ray! .at p .tangent-to c, 1
        t2 = new Ray! .at p .tangent-to c, -1          
        [c, x, t1, t2, p]
      hold: (c, x, t1, t2) ->
        (t1.vector.dot(c.tangent-v(x)) > 0) and
        (t2.vector.dot(c.tangent-v(x)) < 0)
      assuming: [ (c) -> c.is-nontrivial  ]

############################################################

test-segments = 
    name: \Segments
    suite: 
      * name: "is-containing 1"
        for: [ any-segment!, any-num! ]
        assuming: [(s) -> s.is-nontrivial],
        hold: (s, t) ->
          (0 <= t <= 1) `implies` (s .is-containing (s.point t))

      * name: "extension 1"
        for: [ any-segment!, any-num! ]
        hold: (s, n) -> (s.extend(n).length) `equal` (abs(n)*s.length)

      * name: "extension 2",
        for: [ any-segment!, any-num! ],
        hold: (s, n) -> (s.extend(n).extend(n).length) `equal` (n*n*s.length)

      * name: "extension 3"
        for: [ any-segment!, any-num! ]
        assuming: [ (_, n) -> n >= 0 ]
        hold: (s, n) ->
          (s.extend-to-length(n).length) `equal` (if s .is-trivial then 0 else n)
        
      * name: "extension 4"
        for: [any-segment!]
        hold: (s, n) -> s.extend(0) .is-trivial and s.extend-to-length(0) .is-trivial

      * name: "extend-to 1"
        run: -> segment([1 1], [2 3]) .extend-to Ox
        result: ray([1 1], [2 3])

      * name: "extend-to 2"
        for: [ any-segment!, any-line! ]
        hold: (s, l) ->
          h = s.extend-to l
          if h instanceof Segment
            then l .is-containing h.end
            else h instanceof Ray

      * name: "extend-to 3"
        for: [any-point!, any-segment!]
        assuming:
          (p, s) -> s.is-nontrivial
          (p, s) -> ! s.extension .is-containing p
        hold : (p, s) ->
          a = Angle.azimuth(p, s.start)
          c = new Segment! .at p .atAngle a .extend-to s
          c instanceof Segment and c.end .is-equal s.start

      * name: "height-to 1"
        for: [any-segment!, any-ray!]
        with: (s, r) -> [ s, r, s.height-to(r) ]
        hold: (s, r, h) ->
          if h instanceof Segment
            then r .is-containing h.end
            else h instanceof Ray

      * name: "height-to 2"
        for: [ any-segment!, any-line \l ],
        assuming: [ (s, l) -> s.is-nontrivial and l.is-nontrivial ]
        with: (s, l) -> [ s, l, s .height-to l .label \h ],
        hold:  (s, l, h) -> h instanceof Segment and l .is-containing h.end

      * name: "tangent-to 1"
        for: [any-point!, any-circle!]
        with: (p, c) ->
          t1 = new Segment!.at(p).tangent-to(c, 1)
          t2 = new Segment!.at(p).tangent-to(c, -1)
          [p, c, t1, t2]
        hold: (p, c, t1, t2) ->
          (t1.vector.dot(c.tangent-v(c.locus(t1.end))) > 0) and
          (t2.vector.dot(c.tangent-v(c.locus(t2.end))) < 0)
        assuming: [ (p, c) -> c.is-nontrivial and !c.is-enclosing(p) ]

      * name: "tangent-to 2"
        for: [any-circle!, any-param!]
        with: (c, x) ->
          p = c.point x
          t1 = new Segment!.at(p).tangent-to(c, 1)
          t2 = new Segment!.at(p).tangent-to(c, -1)          
          [c, x, t1, t2, p]
        hold: (c, x, t1, t2) ->
          (t1.vector.dot(c.tangent-v(x)) > 0) and
          (t2.vector.dot(c.tangent-v(x)) < 0)
        assuming: [ (c) -> c.is-nontrivial  ]


      * name : "tangent-to 1"
        for : [any-point!, any-circle!]
        with : (p, c) -> [ p, c, new Segment! .at p .tangent-to c ]
        hold : (p, c, t) -> t .is-parallel-to (c.tangent c.locus t.end)
        assuming : [(p, c) -> c.is-nontrivial and ! c.is-enclosing p]

      * name: "tangent-to 2"
        for: [any-point!, any-circle!]
        with: (p, c) ->
          t = new Segment! .at p .tangent-to c
          [p, c, t]
        hold: (p, c, t, i) ->
          (c .isContaining t.end) and
          new Line! .at t.end .perpendicular-to t .isContaining c.center
        assuming: [(p, c) -> c.is-nontrivial and !c .is-enclosing p]

      * name: "tangent-to 3"
        for: [any-circle!, any-param!]
        with: (c, x) -> [c, x, new Segment! .at (c.point x) .tangent-to c ]
        hold: (c, x, t) -> t .is-perpendicular-to (c.radius x)
        assuming: [ (c) -> c.is-nontrivial ]

      * name: \trigonometry
        for: [ any-param(0, 1/4) ]
        with: (x) ->
          C = new Circle 10
          r = C.radius x
          A = C.point x
          s = new Segment \sin .at A .height-to Ox
          c = new Segment \cos .at A .height-to Oy
          t = new Segment \tan .at A .tangent-to(C, -1) .extend-to Ox
          ct = new Segment \cot .at A .tangent-to(C, 1) .extend-to Oy
          phi = deg2rad(Angle.azimuth(origin, A))
          [phi, s, c, t, ct, r, C, Ox, Oy]
        hold:  (phi, s, c, t, ct, r) ->
          (s.length/r.length)  `equal` (sin phi) and
          (c.length/r.length)  `equal` (cos phi) and
          (t.length/r.length)  `equal` (tan phi) and
          (ct.length/r.length) `equal` (ctg phi)
        assuming: [ (x) -> 0 < x < 1/4 ]
        number: 100

############################################################

test-group =
    name: 'Group'
    suite: [  ]

############################################################

test-intersections =
    name: 'Intersections'
    suite: [  ]

############################################################

test-angles =
  name: 'Angle'
  suite: 
    * name: "modulus"
      for: [ any-angle-value! ]
      hold: (a) -> mod(-a, 360) `equal`  mod(360-a, 360)

    * name: "isomorphism 1"
      for: [ any-XY! ]
      hold: (v) -> (Angle.iso(Angle.iso(v).angle).vector `cross` v.normalize!) `equal` 0
      
    * name: "isomorphism 2"
      for: [ any-angle-value! ]
      hold: (a) -> equalMod 360, Angle.iso(Angle.iso(a).vector).angle, a

    * name: "copy 1"
      for: [ any-angle! ]
      hold: (a) ->
        not (a.copy!) == a and (a.copy!).value == a.value

    * name: "isContaining"
      suite: 
        * name: \1, run: -> ! new Angle(30) .isContaining 40
        * name: \2, run: -> new Angle(30) .isContaining 10
        * name: \4, run: -> new Angle(30) .isContaining 0
        * name: \5, run: -> new Angle(30) .isContaining 30
        * name: \6, run: -> ! new Angle(30).rotate(45) .isContaining 30
        * name: \7, run: -> new Angle(30).rotate(45) .isContaining 70
        * name: \8, run: -> ! new Angle(30).rotate(-45) .isContaining 0
        * name: \9, run: -> new Angle(30).rotate(-20) .isContaining 0
        * name: \9, run: -> new Angle(30).rotate(340) .isContaining 0
        * name: \10, run: -> new Angle(30).reflectAt([0 0]) .isContaining 200
        * name: \11, run: -> ! new Angle(30).reflectAt([0 0]) .isContaining 170

############################################################

test-circle =
  name: 'Circle'
  suite:
    * name: 'IntersectionL 1'
      for: [ any-circle!, any-line! ]
      assuming: [ (c, l) -> c.isNontrivial and l.isNontrivial ]
      with: (c, l) -> [ c, l, c.intersectionL(l), (c.center .distance l) ]
      hold: (c, l, i, d) ->
        switch compare(d, c.R)
        | \GT => i.length == 0
        | \EQ => i.length == 1
        | \LT => i.length == 2

    * name: 'isomorphism 1'
      for: [ any-circle!, any-param! ]
      hold: (c, t) -> (c.locus c.point t) `equal` t
      assuming: [ (c) -> c.isNontrivial ]

    * name: 'isomorphism 2'
      for: [ any-circle!, any-param! ]
      with: (c, t) -> [ c, c.point(t), t ]
      hold: (c, p) -> (c.point c.locus p) `equal` p
      assuming: [ (c) -> c.isNontrivial ]

    * name: 'radius',
      suite:
        * name: \1
          run: -> new Circle 3 .radius 0 .end
          result: point [3 0]
        * name: \2
          run: -> new Circle 3 .radius 0.5 .end
          result: point [-3 0]
        * name: \3
          run: -> new Circle 3 .radius 0.25 .end
          result: point [0 3]
        * name: \4
          run: -> new Circle 3 .radius 1 .end
          result: point [3 0]

    * name: 'tangent',
      suite:
        * name: \1
          run: -> new Circle 3 .tangent-v 0
          result : [0 1]
        * name: \2
          run: -> new Circle 3 .tangent-v 1/2
          result : [0 -1]
        * name: \3
          run: -> new Circle 3 .tangent-v 1/4
          result : [-1 0]
        * name: \4
          run: -> new Circle 3 .tangent-v 1/8
          result : [-1 1].normalize!
        * name: \5
          run: -> new Circle 3 .tangent-v 1
          result : [0 1]

############################################################

test-polygon =
  name: 'Polygon'
  suite: [  ]

############################################################

test-square =
  name: 'Square'
  suite: [  ]

############################################################


all-tests =
  number: 25
  log: true
  suite: 
    test-tests
    test-points
    test-lines
    test-rays
    test-segments
    test-intersections
    test-angles
    test-polygon
    test-square
    test-circle

console.log 'Running tests...'
run-tests all-tests
console.log 'Testing done'
