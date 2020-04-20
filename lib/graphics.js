d3.formatDefaultLocale(
    {'decimal':',',
     'thousands':' '})

function SVG(id, opts)
{
    var defaults = {
	'aspectRatio' : 1,
	'size' : 600
    }

    this.options = Object.assign(defaults, opts || {})
    var style = `background: ${this.options['background']};`
    this.width = function () { return this.options.size }
    this.height = function () { return this.options.size*this.options.aspectRatio }

    var res = document.createElementNS('http://www.w3.org/2000/svg',"svg")
    res.setAttribute('id', id)
    res.setAttribute('width', this.width())
    res.setAttribute('height', this.height())
    res.setAttribute('class', "pagebreak")
    res.setAttribute('style', style)
    res.setAttribute('xmlns',"http://www.w3.org/2000/svg")
    document.body.appendChild(res)
    d3.select("#"+id).append('defs').attr('id',"defs")
    d3.selectAll("#defs").html(patterns)

    this.selection = d3.select("#"+id)

    this.svg = res
    
    this.save = function (name) {
	var svgEl = this.svg//gr.svg._groups[0][0]
	svgEl.setAttribute("xmlns", "http://www.w3.org/2000/svg");
	var svgData = svgEl.outerHTML;
	var preface = '<?xml version="1.0" standalone="no"?>\r\n';
	var svgBlob = new Blob([preface, svgData], {type:"image/svg+xml;charset=utf-8"});
	var svgUrl = URL.createObjectURL(svgBlob);
	var downloadLink = document.createElement("a");
	downloadLink.href = svgUrl;
	downloadLink.download = name;
	document.body.appendChild(downloadLink);
	downloadLink.click();
	document.body.removeChild(downloadLink);
    }
}

function Graphics(viewport, opts)
{
    var defaults = {
	'left-margin' : 47,
	'right-margin' : 30,
	'top-margin' : 10,
	'bottom-margin' : 40,
	'xRange'    : 'automatic',
	'yRange'    : 'automatic',
	'xAxisType' : 'linear',
	'yAxisType' : 'linear',
	'xAxis'     : true,
	'yAxis'     : true,
	'class'   : 'graphics',
	'aspectRatio' : 0.61803,
	'size' : 350
    }

    this.save = function (name) {
	this.viewport.save(name)
    }
    
    this.options = Object.assign(defaults,opts || {})

    this.viewport = viewport.selection.append('g').attr('class',this.options['class'])

    this.paper = this.viewport.append('g').attr('class','paper')

    
    this.width = this.options['size'] - this.options['left-margin'] - this.options['right-margin']
    this.height = this.width*this.options['aspectRatio']

    this.viewportWidth = this.options['size']
    this.viewportHeight = this.height + this.options['top-margin'] + this.options['bottom-margin']

    viewport.selection.attr('width',this.viewportWidth)
    viewport.selection.attr('height',this.viewportHeight)

    this.paper
	.attr('transform','translate('+this.options['left-margin']+','+this.options['top-margin']+')')
    
    this.xRange = function (x) {
	this.options.xRange = x || this.options.xRange
	this.X = this.X.domain(this.options.xRange)
	return x && this || this.options.xRange
    }

    this.xAxisType = function (x) {
	this.options.xAxisType = x || this.options.xAxisType
	this.logX = (x == 'log')
	this.X = this.logX ? d3.scaleLog() : d3.scaleLinear()
	this.X = this.X.domain(this.options.xRange).range([0,this.width])
	return x && this || this.options.xAxisType
    }

    this.yRange = function (x) {
	this.options.yRange = x || this.options.yRange
	this.Y = this.Y.domain(this.options.yRange)
	return x && this || this.options.yRange
    }

    this.yAxisType = function (x) {
	this.options.yAxisType = x || this.options.yAxisType
	this.logY = (x == 'log')
	this.Y = this.logY ? d3.scaleLog() : d3.scaleLinear()
	this.Y = this.Y.domain(this.options.yRange).range([this.height,0])
	return x && this || this.options.yAxisType
    }

    this.X = d3.scaleLinear().range([0,this.width])
    this.Y = d3.scaleLinear().range([this.height,0])

    this.logX = false
    this.logY = false
    
    this.rescale = function()
    {
	this.width = this.viewportWidth - this.options['left-margin'] - this.options['right-margin']
	this.height = this.viewportHeight - this.options['top-margin'] - this.options['bottom-margin']    
	this.paper.attr('width',this.width)
	    .attr('height',this.height)
	    .attr('transform','translate('+this.options['left-margin']+','+this.options['top-margin']+')')

	var rx = this.options.xAxisType=='inverted'?this.options.xRange.reversed():this.options.xRange
	var ry = this.options.yAxisType=='inverted'?this.options.yRange.reversed():this.options.yRange

	this.X = this.X.domain(rx).range([0,this.width])
	this.Y = this.Y.domain(ry).range([this.height,0])	
    }

    this.clean = function()
    {
	viewport.selectAll('.'+this.options['class']).remove();
	return this
    }

    this.cleanPaper = function()
    {
	viewport.selectAll('.paper').remove();
	this.paper = this.viewport.append('g').attr('class','paper');
	this.viewportWidth = viewport.attr('width');
	this.viewportHeight = viewport.attr('height');
	
	this.width = this.viewportWidth - this.options['left-margin'] - this.options['right-margin']
	this.height = this.viewportHeight - this.options['top-margin'] - this.options['bottom-margin']    
	this.paper.attr('width',this.width)
	    .attr('height',this.height)
	    .attr('transform','translate('+this.options['left-margin']+','+this.options['top-margin']+')')
	return this
    }
    
    this.add = function (p)
    {
	this.paper.append(p.paper)
	return this
    }

    this.adjustSize = function () {
	var bbox = this.paper.node().getBBox()
	var x = min(bbox.x,viewport.width())
	var y = min(bbox.y,viewport.height())
	var w = min(bbox.width+20,viewport.width())
	var h = min(bbox.height+20,viewport.height())
	this.viewport.attr('transform',`translate(${-x},${-y})`)
	viewport.selection.attr('width',w)
	viewport.selection.attr('height',h)
    }
}

const sups = ["⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"]

function sup(n)
{
    if (n < 0) return "⁻"+sup(-n)
    if (n < 10) return sups[n]
    if (n >= 10) return Array.from((n).toString()).sum(sup,"")
}

var fmt = {
    'fixed1' : d3.format("." + max(0, d3.precisionFixed(0.05) - 1)),
    'fixed2' : d3.format("." + max(0, d3.precisionFixed(0.005) - 1)),
    'percent' : d3.format("." + max(0, d3.precisionFixed(0.05) - 2) + "%"),
    'percent05' : d3.format("." + max(0, d3.precisionFixed(0.05) - 1) + "%"),
    'int' : d3.format('i'),
    'pow10' : pow10,
    'h:mm' : t=>`${floor(t/60)}:${t%60==0?"00":t%60}`,
    'number' : x => x.toString(),
    'degree' : a => d3.format('i')(a) + "°",
}

function pow10(n)
{
    if (n == 1) return 1
    var x = Math.round(log(10,n))
    if (x == 1) return 10
    if (x == 2) return 100
    return "10"+sup(x)
}

Graphics.prototype.clipPaper = function ()
{
    this.viewport.append("rect")
	.attr("x", 0)
	.attr("y", 0)
	.attr("width", this.options['left-margin'])
	.attr("height", this.viewportHeight)
	.attr("class", 'clip')
    this.viewport.append("rect")
	.attr("x", 0)
	.attr("y", 0)
	.attr("width", this.viewportWidth)
	.attr("height", this.options['top-margin'])
	.attr("class", 'clip')
    this.viewport.append("rect")
	.attr("x", 0)
	.attr("y", this.viewportHeight-this.options['bottom-margin'])
	.attr("width", this.viewportWidth)
	.attr("height", this.options['bottom-margin'])
	.attr("class", 'clip')
    this.viewport.append("rect")
	.attr("x", this.viewportWidth-this.options['right-margin'])
	.attr("y", 0)
	.attr("height", this.viewportHeight)
	.attr("width", this.options['right-margin'])
	.attr("class", 'clip')
    return this
}

Graphics.prototype.axes = function (opts)
{
    var defaults = {
	xAxis  : true,
	yAxis  : true,
	xTicks : 10,
	yTicks : 10,
	xLabel : "x",
	yLabel : "y",
	xTickFormat : null,
	yTickFormat : null,
	'class'     : null,
	xTickValues : 'automatic',
	yTickValues : 'automatic',
	yAxesReversed : false
    }
    var options = Object.assign(defaults, opts || {})    

    this.options['bottom-margin']
    this.rescale()
    
    var xAxisPositionX = this.options['left-margin']
    var xAxisPositionY = this.viewportHeight - this.options['bottom-margin']+1
    var yAxisPositionX = this.options['left-margin']-1
    var yAxisPositionY = this.options['top-margin']
    var xAxisLabelPositionX = this.options['left-margin']/4
    var xAxisLabelPositionY = this.viewportHeight - 5
    var yAxisLabelPositionX = 11
    var yAxisLabelPositionY = this.options['top-margin'] + this.height / 2 - 0

    var g = this.viewport.append('g').attr('class','axis')

    if (options.xAxis)
    {
	this.xAxis = d3.axisBottom()
	    .scale(this.X)
	    .tickFormat(options.xTickFormat)
	if (options.xTickValues == 'automatic')
	    this.xAxis = this.xAxis.ticks(options.xTicks)
	else
	    this.xAxis = this.xAxis.tickValues(options.xTickValues)
	
	g.append("g").attr('class','x')
	    .call(this.xAxis)
	    .attr("transform", 'translate(' + xAxisPositionX + ',' + xAxisPositionY +')')

	g.append('g').attr('class','axisLabel')
	    .attr("transform", 'translate(' + xAxisLabelPositionX + ',' + xAxisLabelPositionY + ')')
	    .append("text").attr('class',options['class'])
	    .attr('text-anchor',"middle")
	    .attr('x',"50%")
	    .text(options.xLabel)
	
    }
    if (options.yAxis)
    {
	this.yAxis = d3.axisLeft()
	    .scale(this.Y)
	    .tickFormat(options.yTickFormat)
	if (options.yTickValues == 'automatic')
	    this.yAxis = this.yAxis.ticks(options.yTicks)
	else
	    this.yAxis = this.yAxis.tickValues(options.yTickValues)
	g.append("g").attr('class','y')
	    .call(this.yAxis)
	    .attr("transform", 'translate('+yAxisPositionX+','+yAxisPositionY+')');
	
	g.append('g').attr('class', 'axisLabel')
	    .attr("transform", 'translate(' + yAxisLabelPositionX + ',' + yAxisLabelPositionY + ')')
	    .append("text").attr('class', options['class'])
	    .attr('text-anchor',"middle")
	    .attr('transform', 'rotate(-90)')
	    .text(options.yLabel)
    }

    return this
}

Graphics.prototype.label = function (txt,opts)
{
    var defaults = {
	at       : [0.5*(this.xRange()[0]+this.xRange()[1]),0.5*(this.yRange()[0]+this.yRange()[1])],
	angle    : 0,
	text     : txt,
	'class'  : null,
	'parent' : null,
	'style'  : {}
        }

    var options = Object.assign(defaults, opts || {})    
    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = parent.append('g')
	.attr('transform','translate('+this.X(options.at[0])+','+this.Y(options.at[1])+')')

    g.append('text')
	.text(options.text)
	.attr('transform','rotate(-'+options.angle+')')
	.attr('style',styleString(options['style']))

    return this

}

Graphics.prototype.listLinePlot = function (d, opts)
{
    defaults ={
	'class' : 'plot',
	'points' : false,
	'joined' : true,
	'parent' : null
    }
    var options = Object.assign(defaults, opts || {})
    return this.listPlot(d,options)
}

Graphics.prototype.listStairsPlot = function (d, opts)
{
    if (d[0].length==1)
	pts = d.mapappend((x,i) => [[x,i],[x,i+1]])
    else
    {
	var pts = [d[0]]
	for(var i =1;i<d.length;i++)
	{
	    pts.push([d[i][0],d[i-1][1]])
	    pts.push([d[i][0],d[i][1]])
	}
    }
    
    return this.listLinePlot(pts,opts)
}

Graphics.prototype.listPlot = function (d, opts)
{
    var defaults = {
	'points' : true,
	'marker' : false,
	'joined' : false,
	'filled' : false,
	'filledUp' : false,
	'needles' : false,
	'pointSize' : 3,
	'parent' : null,
	'arrow': false,
	'point-style' : {'stroke-width':'0.5', 'stroke':'black', 'fill':'yellow'},
	'line-style': {'stroke-width':'1', 'stroke':'black', 'fill':'none'}
    }

    var options = Object.assign(defaults, opts)

    var data = d
    
    if (typeof data[0] == 'number')
	data = range(1,data.length+1).zip(data)
	    
    if (this.xRange() == 'automatic')
	this.xRange(d3.extent(data, d => d[0]))
    if (this.yRange() == 'automatic')
	this.yRange(d3.extent(data, d => d[1]))

    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = parent.append('g')
  
    if (options.filled)
    {
	var area = d3.area().x(d=>this.X(d[0])).y0(this.height).y1(d => this.Y(d[1]))
	g.append('path').attr('class','area')
	    .attr('stroke', 'none')
	    .attr('d', area(data));
    }
    
    if (options.joined)
    {
	var line = d3.line().x(d => this.X(d[0])).y(d => this.Y(d[1]))
	var l = g.append('path')
	    .attr('d', line(data))
	    .attr('style', styleString(options['line-style']))

	if (options.arrow)
	    l.attr('marker-end',"url(#Triangle)");
    }

    if (options.points)
    {
	var dots = g.append('g')
	dots.selectAll('circle')
	    .data(data)
	    .enter()
	    .append('circle')
	    .attr('cx', d => this.X(d[0]))
	    .attr('cy', d => this.Y(d[1]))
	    .attr('r', options.pointSize)
	    .attr('style',styleString(options['point-style']))
    }

    if (options.marker)
    {
	var dots = g.append('g').attr('class','marker')
	dots.selectAll('text')
	    .data(data)
	    .enter()
	    .append('text')
	    .attr('text-anchor',"middle")
	    .text(options.marker)
	    .attr('x', d => this.X(d[0]))
	    .attr('y', d => this.Y(d[1])+6)
    }


    return this
}

var marker = ["▫","+","×"]

Graphics.prototype.gridLines = function (opts)
{
    var defaults = {
	'x'      : null,
	'y'      : null,
	'class'  : null,
	'parent' : null
        }

    var options = Object.assign(defaults, opts || {})    

    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = parent.append('g').attr('class','gridLine')

    if (options.x)
    {
	var dots = g.append('g').attr('class','x')
	dots.selectAll('line')
	    .data(options.x)
	    .enter()
	    .append('line')
	    .attr('x1', this.X)
	    .attr('y1', 0)
	    .attr('x2', this.X)
	    .attr('y2', this.height)
    }
    if (options.y)
    {
	var dots = g.append('g').attr('class','y')
	dots.selectAll('line')
	    .data(options.y)
	    .enter()
	    .append('line')
	    .attr('x1', 0)
	    .attr('y1', y => this.Y(y) + 0.5)
	    .attr('x2', this.width)
	    .attr('y2', y => this.Y(y) + 0.5)
    }
    return this
}

Graphics.prototype.discretePlot = function (f,opts) {
    var defaults = {
	'class' : 'discretePlot',
	'kind' : 'needles',
	'parent' : null,
	'step' : 1
    }
    var options = Object.assign(defaults, opts || {})
    var data = range(this.xRange()[0],this.xRange()[1],options.step)
	.map(x => [x,f(x)])
    if (options['kind'] == 'needles') {
	return this.listPlot(data, Object.assign(options,{'needles':true}))
    }
    else if (options['kind'] == 'stairs') {
	return this.plot(x => f(floor(x)),Object.assign(opts,{'minAngle':0}))
    }
}


Graphics.prototype.polarPlot = function (f,opts) {
    var defaults = {
	'class' : 'plot',
	'points' : false,
	'joined' : true,
	'filled' : false,
	'pointsize' : 3,
	'parent' : null,
	'plotPoints' : 360,
	'domain' : [0,2*pi]
    }
    var options = Object.assign(defaults, opts || {})
    var step = (options.domain[1]-options.domain[0])/options.plotPoints
    data = range(options.domain[0],options.domain[1],step)
	.map(a => [f(a)*cos(a), f(a)*sin(a)])
    return this.listPlot(data, options)
    
}

Graphics.prototype.plot = function (f,opts) {

    var defaults = {
	'class' : 'plot',
	'points' : false,
	'joined' : true,
	'filled' : false,
	'filledUp' : false,
	'pointsize' : 3,
	'parent' : null,
	'plotPoints' : 35,
	'maxIteration' : 20,
	'maxAngle' : 0.07,
	'minAngle' : 0.0001,
	'minDistance' : 0.0001,
	'domain' : null,
	'marks' : []
    }

   
    var options = Object.assign(defaults, opts || {})
    var x1,x2,tbl
    [x1,x2] = options.domain || this.xRange()
    var X = this.X, Y = this.Y
    var dx = (x2-x1)/options.plotPoints
    var tbl = []
    for(var x = x1; x <= x2+dx; x+=dx)
	tbl.push([x,f(x)])
    tbl = findBreaks(tbl)
    tbl=fixedPoint(refine,tbl,options.maxIteration,x => x.length)
    var res = this.listPlot(tbl,options)
    if (options.marks.length > 0)
	res = res.listPlot(options.marks.map(x => [x,f(x)]),{'pointSize' : 4,
					  'class' : 'marks'})
    return res

    function findBreaks(lst) {
	var res = []
	if (!isNaN(lst[0][1]))
	    res.push(lst[0])
	for(var i = 1; i < lst.length; i++) {
	    if (isNaN(lst[i-1][1]) ^ isNaN(lst[i][1]))
		res.push(bisection(f, lst[i-1][0], lst[i][0], isNaN, 1e-13))
	    res.push(lst[i])
	}
	return res
    }

    function refine (tbl) {
	const maxAngle = 1-cos(options.maxAngle)
	const minAngle = 1-cos(options.minAngle)
	var res = [tbl[0]], p1,p2,p3,x
	for(var i = 1; i < tbl.length-1; i++) {
	    p1 = tbl[i-1]
	    p2 = tbl[i]
	    p3 = tbl[i+1]
	    if (dist(p2,p1) < options.minDistance || dist(p2,p3) < options.minDistance) {
		res.push(p2)
		continue
	    }
	    if (cosBetween(p1,p2,p3) > maxAngle)
	    {
		x = (p1[0]+2*p2[0])/3
		res.push([x, f(x)])
		res.push(p2)
		x = (p3[0]+2*p2[0])/3
		res.push([x, f(x)])
	    }
	    else if (cosBetween(p1,p2,p3) > minAngle)
		res.push(p2)
	}
	res.push(tbl[tbl.length-1])
	return res
    }

    function dist(p1,p2) {
	var x = X(p2[0])-X(p1[0])
	var y = Y(p2[1])-Y(p1[1])
	return x*x+y*y
    }

    function cosBetween(p1,p2,p3) {
	const x12 = X(p2[0])-X(p1[0])
	const x23 = X(p3[0])-X(p2[0])
	const y12 = Y(p2[1])-Y(p1[1])
	const y23 = Y(p3[1])-Y(p2[1])
	return 1-(x12*x23 + y12*y23)/sqrt((x12*x12+y12*y12)*(x23*x23+y23*y23))
}

}

Graphics.prototype.rectangle = function (x,y,w,h,opts)
{
    var defaults = {
	cornerRadius:0,
	'class'  : null,
	'parent' : null,
	'fill'   : null,
	'stroke' : null,
	'opacity': null
    }

    var options = Object.assign(defaults, opts || {})    

    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = this.paper.append("rect")
	.attr("x",this.X(x))
	.attr("y",this.Y(y+h))
	.attr("rx",options.cornerRadius)
	.attr("width",this.X(x+w)-this.X(x))
	.attr("height",this.Y(y-h) - this.Y(y))
    if (options['fill'])
    	g.attr("fill",options['fill'])
    if (options['stroke'])
    	g.attr("stroke",options['stroke'])
    if (options['opacity'])
    	g.attr("opacity",options['opacity'])
    g.attr("class",options['class'])
    return this
}

Graphics.prototype.disk = function ([x,y],r,opts)
{
    var defaults = {
	'parent' : null
        }

    var options = Object.assign(defaults, opts || {})    
    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = this.paper.append('circle')
	.attr("cx",this.X(x))
	.attr("cy",this.Y(y))
	.attr("r",this.X(r)-this.X(0))
	.attr('style',styleString(options['style']))
    return this
}

Graphics.prototype.line = function (pts,opts)
{
    var defaults = {
	'parent' : null,
	'joined':true,
	'points':false
    }

    var options = Object.assign(defaults, opts || {})    

    return this.listPlot(pts,options)
}

Graphics.prototype.arrow = function (pts,opts)
{
    var defaults = {
	'parent' : null,
	'joined':true,
	'points':false,
	'arrow':true
    }

    var options = Object.assign(defaults, opts || {})    

    return this.listPlot(pts,options)
}

Graphics.prototype.arc = function ([x,y],r,a1,a2,opts)
{
    var defaults = {
	'class'  : null,
	'parent' : null
        }

    var options = Object.assign(defaults, opts || {})    

    var parent = options.parent && d3.select(options.parent) || this.paper

    if (a1 > a2) {a2 += 360}
    pts = range(a1,a2+2,2).map(a => [x+r*cos(deg2rad(a)),y+r*sin(deg2rad(a))])

    return this.listLinePlot(pts,options)
}


Graphics.prototype.image = function (src,x,y,w,h,opts)
{
    var defaults = {
	cornerRadius:0,
	'class'  : null,
	'parent' : null
        }

    var options = Object.assign(defaults, opts || {})    
    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = this.paper.append("rect")
    this.paper.append("image")    
	.attr('xlink:href',src)
	.attr('x',(x || 0))
	.attr('y',(y || 0))
	.attr('width',w ||"100%")
	.attr('height',h||"100%")
    return this
}


var patterns = `<style>marker {fill:wheat;}</style>
<pattern id="h1" width="5px" height="5px" viewBox="0,0,20,20" patternUnits="userSpaceOnUse">
<line class="hatch" x1="1" y1="20" x2="20" y2="1" />
<line class="hatch" x1="0" y1="1" x2="1" y2="0" />
</pattern>
<pattern id="h2" width="5px" height="5px" viewBox="0,0,20,20" patternUnits="userSpaceOnUse">
<line class="hatch" x1="0" y1="20" x2="20" y2="0" />
<line  class="hatch" x1="0" y1="0" x2="20" y2="20" />
</pattern>
<marker id="Triangle" viewBox="0 0 10 8" refX="8" refY="4"
    markerWidth="6" markerHeight="6" orient="auto">
  <path d="M 0 0 L 10 4 L 0 8 z" />
</marker>
<marker id="ArrowHead" viewBox="0 0 10 8" refX="8" refY="4"
    markerWidth="6" markerHeight="6" orient="auto">
  <path d="M 0 0 L 10 4 L 0 8 z" />
</marker>`

function styleString(so) {
    var res = ""
    for(k in so)
	res += `${k}:${so[k]}; `
    return res
}
