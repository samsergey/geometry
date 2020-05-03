labeled = (l) -> (f) -> (...x) -> if l then f(...x).label(l) else f(...x)

anyParam = (a = 0, b = 1) ->
  anyNum()
  .range(a, b)
  .precision(0.01)
  .ascending()

############################################################
/** abc 
 * cf
 * 
 */
testTests =
  name: \Tests
  skip: yes
  log: on
  suite: 
    * name: \1
      for:
        anyNum().range(0,100).precision(0.1)
      hold: (n) -> n == 4
      number: 10
      
    * name: \2
      for:
        anyNum()
        anyNum()
      hold: (n, m) -> n >= m
      log: off
      
    * name: \3
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
        c -> c.isNontrivial

    * name: "trigonometry"
      for:
        anyParam(0, 1/4)
      with: x ->
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
      assuming : [x -> x > 0.1 && x < 0.2]

f = ->
  x = 8
  y = x + 7
  x+y
     

############################################################

[a
 b
 c]
