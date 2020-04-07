function Chart(f)
{
    this.paper = new Graphics(createSVG(f),{'size':500,'aspectRatio':1/2,
			   'left-margin':10,'right-margin':10,
			   'top-margin':10,'botom-margin':10,
			   'class':'slide'})
	.xRange([-20,20])
    	.yRange([-10,10])

    this.put = function (objects)
    {
	objects.forEach(o => o.show(this.paper))
    }
}

function Point([x,y],label)
{
    this.pt = [x,y]
    this.x = x
    this.y = y
    this.label = label
    this.setLabel = function (l)
    {
	this.label = l
	return this
    }
    var label_pt = this.pt.vadd([-0.5,-0.5])
    var labelOffset
    this.labelOffset = function (o)
    {
	labelOffset = o.normalize().scale(1.2)
	return this
    }
    this.labelOffset([1,0])
    var ox = 1, oy = 1, a = 120
    this.show = function (p)
    {
	p.listPlot([[x,y]])
	if (this.label)
	{
	    p.label(this.label,{'at':label_pt.vadd(labelOffset)})
	}
    }
}

function Segment(p1,p2,label)
{
    this.start = p1.pt
    this.end = p2.pt
    this.eqn = lineEquation(this.start, this.end)
    this.middle = [(p1.x+p2.x)/2,(p1.y+p2.y)/2]
    this.vector = [(p2.x-p1.x),(p2.y-p1.y)]
    this.length = this.vector.norm()
    this.unit = this.vector.normalize()
    this.norm = [-this.unit[1],this.unit[0]]
    this.label = label

    var label_pt
    this.labelPosition = function (t)
    {
	label_pt = this.eqn(t*this.length).vadd([-0.5,-0.5])
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
	p.listLinePlot([p1.pt,p2.pt])
	if (this.label)
	{
	    p.label(this.label,{'at':label_pt.vadd(labelOffset)})
	}
    }
}

function lineEquation([x1,y1],[x2,y2])
{
    var v = [x2-x1,y2-y1].normalize()
    return t => [x1,y1].vadd(v.scale(t))
}

function Ray(p1,p2,label)
{
    this.start = p1.pt
    this.eqn = lineEquation(p1.pt,p2.pt)
    this.end = this.eqn(100)
    this.unit = [p2.x-p1.x, p2.y-p1.y].normalize()
    this.norm = [-this.unit[1],this.unit[0]]
    this.label = label

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
	p.listLinePlot([this.start,this.end])
	if (this.label)
	{
	    p.label(this.label,{'at':label_pt.vadd(labelOffset)})
	}
    }
    
}

function CoordRay(p1,n)
