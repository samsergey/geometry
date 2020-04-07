function Chart(f)
{
    this.paper = new Graphics(createSVG(f),{'size':500,'aspectRatio':1,
			   'left-margin':10,'right-margin':10,
			   'top-margin':10,'botom-margin':10,
			   'class':'chart'})
	.xRange([-20,20])
    	.yRange([-20,20])

    this.put = function (objects)
    {
	objects.flat().forEach(o => o.show(this.paper))
	return this
    }
}

function Point(pt,label,opts)
{
    var defaults = {
	'pointSize' : 2
    }
  
    this.options = Object.assign(defaults,opts || {})
    this.on = function (line,t)
    {
	if (Number.isFinite(t))
	    return new Point(line.eqn(t))
	if (Array.isArray(t))
	    return t.map(i => new Point().on(line,i))
    }

    this.at = function (pt)
    {
	return new Point(pt,this.label)	
    }

    if (!pt) return
    
    var x = pt[0], y = pt[1]
    this.pt = [x,y]
    this.x = x
    this.y = y
    this.label = ''
    
    var label_pt
    this.setLabel = function (l)
    {
	this.label = l
	label_pt = this.pt.vadd([-0.3*l.toString().length,-0.5])
	return this
    }
    this.setLabel(label || '')
    
    var labelOffset
    this.labelOffset = function (o)
    {
	labelOffset = o.normalize().scale(1.2)
	return this
    }
    this.labelOffset([0,-1])

    var ox = 1, oy = 1, a = 120
    this.show = function (p)
    {
	p.listPlot([[x,y]],opts)
	if (this.label)
	{
	    p.label(this.label,{'at':label_pt.vadd(labelOffset)})
	}
    }

    this.transform = function (T)
    {
    }
}

// линии (отрезки и лучи) параметризованы в масштабе задающих из точек
// если луч задан точкой и углом, то он имеет единичный масштаб.

function Segment(p1,p2,label,opts)
{
    var defaults = {
	'arrow' : false,
	'arrowPosition' : 1
    }
  
    this.options = Object.assign(defaults,opts || {})
    this.start = p1.pt
    this.end = p2.pt
    this.eqn = lineEquation(this.start, this.end)
    this.middle = [(p1.x+p2.x)/2,(p1.y+p2.y)/2]
    this.vector = [(p2.x-p1.x),(p2.y-p1.y)]
    this.length = this.vector.norm()
    this.unit = this.vector.normalize()
    this.norm = [-this.unit[1],this.unit[0]]
    this.label = label
    this.arrow = false

    this.setScale = function (s)
    {
	var p1 = this.eqn(0)
	var p2 = this.eqn(1)
	this.eqn = lineEquation(p1,p2,s)
	return this
    }
    
    var label_pt
    this.labelPosition = function (t)
    {
	label_pt = this.eqn(t).vadd([-0.5,-0.5])
	return this
    }
    this.labelPosition(0.5)
    
    var labelOffset
    this.labelOffset = function (o)
    {
	labelOffset = o.normalize().scale(1.25)
	return this
    }
    this.labelOffset(this.norm)
    
    this.show = function (p)
    {
	p.line([p1.pt,p2.pt],this.options)
	if (this.label)
	{
	    p.label(this.label,{'at':label_pt.vadd(labelOffset)})
	}
	if (this.options['arrow'])
	{
	    var x = this.options['arrowPosition']
	    p.arrow([this.eqn(x - 0.1),this.eqn(x)],this.options)
	}
    }
}

function lineEquation([x1,y1],[x2,y2],s)
{
    var v = [x2-x1,y2-y1], s = s || 1
    return t => [x1,y1].vadd(v.scale(t*s))
}

function Ray(p1,p2,label,opts)
{
    var defaults = {
	'arrow' : false,
	'arrowPosition' : 1
    }

    this.options = Object.assign(defaults,opts || {})
    this.start = p1.pt
    this.eqn = lineEquation(p1.pt,p2.pt)
    this.end = this.eqn(100)
    this.unit = [p2.x-p1.x, p2.y-p1.y].normalize()
    this.norm = [-this.unit[1],this.unit[0]]
    this.label = label

    this.setScale = function (s)
    {
	var p1 = this.eqn(0)
	var p2 = this.eqn(1)
	this.eqn = lineEquation(p1,p2,s)
	return this
    }

    var label_pt
    this.labelPosition = function (t)
    {
	label_pt = this.eqn(t).vadd([-0.5,-0.5])
	return this
    }
    this.labelPosition(3)

    var labelOffset
    this.labelOffset = function (o)
    {
	labelOffset = o.normalize().scale(1.25)
	return this
    }
    this.labelOffset(this.norm)
 
    this.show = function (p)
    {
	p.listLinePlot([this.start,this.end],this.options)
	if (this.label)
	{
	    p.label(this.label,{'at':label_pt.vadd(labelOffset)})
	}
	if (this.options['arrow'])
	{
	    var x = this.options['arrowPosition']
	    p.arrow([this.eqn(x - 0.1),this.eqn(x)],this.options)
	}

    }

}

function RayThrough(p,a,label,opts)
{
    var r = deg2rad(a)
    var p2 = new Point(p.pt.vadd([cos(r),sin(r)]))
    return new Ray(p,p2,label,opts) 
}

function CoordRay(p,n,opts)
{
    var defaults = {
	'labelPosition' : [0,-1],
    }

    this.options = Object.assign(defaults,opts || {})
    var O = p.setLabel('O')
	.labelOffset(this.options['labelPosition'])
    var r = new RayThrough(O,0,'',{'arrow':true,
				   'arrowPosition':n+1.5})
	.setScale(36/n)
    var Ps = new Point().on(r, range(0,n+1))
	.map((p,i) => p.setLabel(i)
	     .labelOffset(this.options['labelPosition']))
    return [O, r, Ps]
}

function PolyLine(ps,opts)
{
    var defaults = {
	'labelPosition' : [0,-1],
    }
    this.options = Object.assign(defaults,opts || {})

    this.points = ps
    this.show = function (p)
    {
	p.listLinePlot(ps.map(p => p.pt),this.options)
    }

    
}

function TriangleV(A,B,C,opts)
{
    
}

function Triangle(a,b,c,opts)
{
    
}

function TriangleR(a,b,opts)
{
    
}
