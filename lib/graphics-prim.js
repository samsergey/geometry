function Graphics(viewport, opts)
{
    var defaults = {
	margin    : {left : 55, right : 30, top : 30, bottom : 60}, 
	xRange    : 'automatic',
	yRange    : 'automatic',
	xAxisType : 'linear',
	yAxisType : 'linear',
	xAxis     : true,
	yAxis     : true,
	'class'   : 'graphics'
    }
    this.options = Object.assign(defaults,opts || {})

    this.viewport = viewport.append('g').attr('class',this.options['class'])
    this.paper = this.viewport.append('g').attr('class','paper')

    this.viewportWidth = viewport.attr('width');
    this.viewportHeight = viewport.attr('height');
    
    this.width = this.viewportWidth - this.options.margin.left - this.options.margin.right
    this.height = this.viewportHeight - this.options.margin.top - this.options.margin.bottom    
    this.paper.attr('width',this.width)
	.attr('height',this.height)
	.attr('transform','translate('+this.options.margin.left+','+this.options.margin.top+')')
    
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
	this.width = this.viewportWidth - this.options.margin.left - this.options.margin.right
	this.height = this.viewportHeight - this.options.margin.top - this.options.margin.bottom    
	this.paper.attr('width',this.width)
	    .attr('height',this.height)
	    .attr('transform','translate('+this.options.margin.left+','+this.options.margin.top+')')
	
	this.X = this.X.domain(this.options.xRange).range([0,this.width])
	this.Y = this.Y.domain(this.options.yRange).range([this.height,0])	
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
	
	this.width = this.viewportWidth - this.options.margin.left - this.options.margin.right
	this.height = this.viewportHeight - this.options.margin.top - this.options.margin.bottom    
	this.paper.attr('width',this.width)
	    .attr('height',this.height)
	    .attr('transform','translate('+this.options.margin.left+','+this.options.margin.top+')')
	return this
    }
    
    this.add = function (p)
    {
	this.paper.append(p.paper)
	return this
    }
}

var fmt = {
    'normal' : null,
    'percent' : d3.format("." + max(0, d3.precisionFixed(0.05) - 2) + "%"),
    'int' : d3.format('i')
}

Graphics.prototype.clipPaper = function ()
{
    this.viewport.append("rect")
	.attr("x", 0)
	.attr("y", 0)
	.attr("width", this.options.margin.left)
	.attr("height", this.viewportHeight)
	.attr("class", 'clip')
    this.viewport.append("rect")
	.attr("x", 0)
	.attr("y", 0)
	.attr("width", this.viewportWidth)
	.attr("height", this.options.margin.top)
	.attr("class", 'clip')
    this.viewport.append("rect")
	.attr("x", 0)
	.attr("y", this.viewportHeight-this.options.margin.bottom)
	.attr("width", this.viewportWidth)
	.attr("height", this.options.margin.bottom)
	.attr("class", 'clip')
    this.viewport.append("rect")
	.attr("x", this.viewportWidth-this.options.margin.right)
	.attr("y", 0)
	.attr("height", this.viewportHeight)
	.attr("width", this.options.margin.right)
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
	xTickValues : null,
	yTickValues : null,
    }
    var options = Object.assign(defaults, opts || {})    

    this.options.margin.bottom
    this.rescale()
    
    var xAxisPositionX = this.options.margin.left
    var xAxisPositionY = this.viewportHeight - this.options.margin.bottom+1
    var yAxisPositionX = this.options.margin.left-1
    var yAxisPositionY = this.options.margin.top
    var xAxisLabelPositionX = this.options.margin.left + this.width / 2
    var xAxisLabelPositionY = this.viewportHeight - this.options.margin.bottom / 3
    var yAxisLabelPositionX = 12 + 0*this.options.margin.left
    var yAxisLabelPositionY = this.options.margin.top + this.height / 2 

    var g = this.viewport.append('g').attr('class','axis')

    if (options.xAxis)
    {
	this.xAxis = d3.axisBottom()
	    .scale(this.X)
	    .tickFormat(options.xTickFormat)
	if (options.xTickValues == null)
	    this.xAxis = this.xAxis.ticks(options.xTicks)
	else
	    this.xAxis = this.xAxis.tickValues(options.xTickValues)
	g.append("g").attr('class','x')
	    .call(this.xAxis)
	    .attr("transform", 'translate(' + xAxisPositionX + ',' + xAxisPositionY +')')

	g.append('g').attr('class','axisLabel')
	    .attr("transform", 'translate(' + xAxisLabelPositionX + ',' + xAxisLabelPositionY + ')')
	    .append("text").attr('class',options['class'])
	    .attr('dx',-options.xLabel.length*4)
	    .text(options.xLabel)
    }
    if (options.yAxis)
    {
	this.yAxis = d3.axisLeft()
	    .scale(this.Y)
	    .tickFormat(options.yTickFormat)
	if (options.xTickValues == null)
	    this.yAxis = this.yAxis.ticks(options.yTicks)
	else
	    this.yAxis = this.yAxis.tickValues(options.yTickValues)
	g.append("g").attr('class','y')
	    .call(this.yAxis)
	    .attr("transform", 'translate('+yAxisPositionX+','+yAxisPositionY+')');
	
	g.append('g').attr('class', 'axisLabel')
	    .attr("transform", 'translate(' + yAxisLabelPositionX + ',' + yAxisLabelPositionY + ')')
	    .append("text").attr('class', options['class'])
	    .attr('dx', -options.yLabel.length*4)
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
	text     : txt || "label",
	'class'  : null,
	'parent' : null
        }

    var options = Object.assign(defaults, opts || {})    

    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = parent.append('g').attr('class','label')
	.attr('transform','translate('+this.X(options.at[0])+','+this.Y(options.at[1])+')')

    g.append('text').attr('class',options['class'])
	.text(options.text)
	.attr('transform','rotate(-'+options.angle+')')

    return this

}


Graphics.prototype.listPlot = function (d, opts)
{
    var defaults = {
	'class' : 'listPlot',
	'points' : true,
	'joined' : false,
	'filled' : false,
	'filledUp' : false,
	'needles' : false,
	'pointSize' : 2,
	'parent' : null
    }

    var options = Object.assign(defaults, opts || {})

    var data = d
    
    if (typeof data[0] == 'number')
	data = range(1,data.length+1).zip(data)
	    
    if (this.xRange() == 'automatic')
	this.xRange(d3.extent(data, d => d[0]))
    if (this.yRange() == 'automatic')
	this.yRange(d3.extent(data, d => d[1]))

    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = parent.append('g').attr('class',options['class'])

    if (options.filled)
    {
	var area = d3.area().x(d=>this.X(d[0])).y0(this.height).y1(d => this.Y(d[1]))
	g.append('path').attr('class','area')
	    .attr('stroke', 'none')
	    .attr('d', area(data));
    }

    if (options.filledUp)
    {
	var area = d3.area().x(d=>this.X(d[0])).y0(0).y1(d => this.Y(d[1]))
	g.append('path').attr('class','area')
	    .attr('stroke', 'none')
	    .attr('d', area(data));
    }

    if (options.needles)
    {
	var needles = g.append('g').attr('class','needles')
	needles.selectAll('line')
	    .data(data)
	    .enter()
	    .append('line')
	    .attr('x1', d => this.X(d[0]))
	    .attr('y1', d => this.logY ? this.height : this.Y(0))
	    .attr('x2', d => this.X(d[0]))
	    .attr('y2', d => this.Y(d[1]))
    }
    
    if (options.joined)
    {
	var line = d3.line().x(d=>this.X(d[0])).y(d => this.Y(d[1]))
	g.append('path').attr('class','line')
	    .attr('fill', 'none')
	    .attr('d', line(data));
    }

    if (options.points)
    {
	var dots = g.append('g').attr('class','points')
	dots.selectAll('circle')
	    .data(data)
	    .enter()
	    .append('circle')
	    .attr('cx', d => this.X(d[0]))
	    .attr('cy', d => this.Y(d[1]))
	    .attr('r', options.pointSize)
    }


    return this
}

Graphics.prototype.diffListPlot = function(dataA, dataB, opts)
{
    var defaults = {
	'class' : null,
	'points' : false,
	'joined' : true,
	'filled' : true,
	'pointsize' : 3,
	'parent' : null
    }
    var options = Object.assign(defaults, opts || {})
    var A = dataA, B = dataB, X = this.X, Y = this.Y
    
    if (typeof A[0] == 'number')
	A = range(1,A.length+1).zip(A)
    if (typeof B[0] == 'number')
	B = range(1,B.length+1).zip(B)

    if (this.xRange() == 'automatic')
	this.xRange(d3.extent(d3.extent(A,x => x[0]).concat(d3.extent(B,x => x[0]))))
    if (this.yRange() == 'automatic')
	this.yRange(d3.extent(d3.extent(A,x => x[1]).concat(d3.extent(B,x => x[1]))))

    function mkarea(ref, D)
    {
	var res = [[D[0][0],ref]].concat(D)
	res.push([D[D.length-1][0],ref])
	return d3.line()
	    .x(d=>X(d[0]))
	    .y(d=>Y(d[1]))(res)
    }
    
    var parent = options.parent && d3.select(options.parent) || this.paper
    var defs = parent.append("defs")
    var g = parent.append('g').attr('class','diffListPlot')
    g = g.append('g').attr('class',options['class'])

    if (options.filled)
    {
	defs.append("clipPath")
	    .attr("id","A")
	    .append("path")
	    .attr("d", mkarea(this.yRange()[0],A));
	g.append("path").attr('class','areaBelow')
 	    .attr("clip-path", "url(#A)")
	    .attr("d", mkarea(this.yRange()[1],B));

	defs.append("clipPath")
	    .attr("id","B")
	    .append("path")
	    .attr("d", mkarea(this.yRange()[0],B));
	g.append("path").attr('class','areaAbove')
	    .attr("clip-path", "url(#B)")
	    .attr("d", mkarea(this.yRange()[1],A));
  
    }

    this.listPlot(A, {'class':'A',
		      'points' : options.points,
		      'joined' : options.joined,
		      'parent' : '.'+options['class'] || '.diffListPlot'})
    this.listPlot(B, {'class':'B',
		      'points' : options.points,
		      'joined' : options.joined,
		      'parent' : '.'+options['class'] || '.diffListPlot'})

    return this
}


Graphics.prototype.diffListPlot1 = function(dataA, dataB, opts)
{
    var defaults = {
	'class' : null,
	'points' : false,
	'joined' : true,
	'filled' : true,
	'pointsize' : 3,
	'parent' : null
    }
    var options = Object.assign(defaults, opts || {})
    var A = dataA, B = dataB
    
    if (typeof A[0] == 'number')
	A = d3.zip(d3.range(1,A.length+1),A)
    if (typeof B[0] == 'number')
	B = d3.zip(d3.range(1,B.length+1),B)

    if (this.xRange() == 'automatic')
	this.xRange(d3.extent(d3.extent(A,x => x[0]).concat(d3.extent(B,x => x[0]))))
    if (this.yRange() == 'automatic')
	this.yRange(d3.extent(d3.extent(A,x => x[1]).concat(d3.extent(B,x => x[1]))))
   
    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = parent.append('g').attr('class','diffListPlot1')
    g = g.append('g').attr('class',options['class'])

    var data = A.zip(B)
    
    if (options.filled)
    {
	var above = g.append('g').attr('class','above')
	above.selectAll('line')
	    .data(data)
	    .enter()
	    .append('line')
	    .attr('x1', ([[xa,ya],[xb,yb]]) => this.X(xa))
	    .attr('y1', ([[xa,ya],[xb,yb]]) => this.Y(ya))
	    .attr('x2', ([[xa,ya],[xb,yb]]) => this.X(xa))
	    .attr('y2', ([[xa,ya],[xb,yb]]) => this.Y(min(ya,yb)))
	var below = g.append('g').attr('class','below')
	below.selectAll('line')
	    .data(data)
	    .enter()
	    .append('line')
	    .attr('x1', ([[xa,ya],[xb,yb]]) => this.X(xa))
	    .attr('y1', ([[xa,ya],[xb,yb]]) => this.Y(ya))
	    .attr('x2', ([[xa,ya],[xb,yb]]) => this.X(xa))
	    .attr('y2', ([[xa,ya],[xb,yb]]) => this.Y(max(ya,yb)))
    }

    this.listPlot(A, {'class':'A',
		      'points' : options.points,
		      'joined' : options.joined,
		      'parent' : '.'+options['class'] || '.diffListPlot'})
    this.listPlot(B, {'class':'B',
		      'points' : options.points,
		      'joined' : options.joined,
		      'parent' : '.'+options['class'] || '.diffListPlot'})
    
    return this
}


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
	    .attr('y1', this.Y)
	    .attr('x2', this.width)
	    .attr('y2', this.Y)
    }
    return this
}


Graphics.prototype.histogram = function(hist, opts)
{
    var defaults = {
	'type'   : 'value',
	'kind'   : 'bars',
	'class'  : 'histogram',
	'parent' : null,
	'barWidth' : 0.95
        }

    var options = Object.assign(defaults, opts || {})
    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = parent.append('g').attr('class',options['class'])

    var value = {
	'PDF' : x => hist.PDF(x),
	'CDF' : x => hist.CDF(x),
	'value' : x => hist.data[x]
    }[options.type]

    
    if (this.xRange() == 'automatic')
    {
	if (this.logX)
	    this.xRange([max(hist.minBin,hist.step), hist.maxBin])
	else
	    this.xRange([hist.minBin-hist.step/2, hist.maxBin+hist.step/2])
    }
    if (this.yRange() == 'automatic')
	this.yRange([this.logY ? value(hist.antimode) : 0,
		     options.type == 'CDF' ? 1 : value(hist.mode)])
	
    var data = []

    var bw = options.barWidth * (this.logX ? 1 : this.X(hist.step)-this.X(0))

    hist.bins.forEach(b => {
	if (!(this.logX && (b <= 0)))
	    data.push({'bin' : Number(b)-options.barWidth*hist.step/2, 'value' : value(Number(b))})})

    if (options.kind == 'bars')
    {
	g.selectAll("bar")
	.data(data)
	.enter().append("rect")
	.attr("x", d => this.X(d.bin))
	.attr("width", bw)
	.attr("y", d => min(this.Y(this.logY ? 1 : 0),this.Y(d.value)))
	.attr("height", d => abs(this.Y(this.logY ? 1 : 0) - this.Y(d.value)))
    }
    else if (options.kind == 'needles')
    {
	g.selectAll("line")
	    .data(data)
	    .enter().append("line").attr('class','needles')
	    .attr("x1", d => this.X(d.bin))
	    .attr("y1", d => this.Y(this.logY ? 1 : 0))
	    .attr("x2", d => this.X(d.bin))
	    .attr("y2", d => this.Y(d.value))
	
	g.selectAll("circle")
	    .data(data)
	    .enter().append("circle").attr('class','point')
	    .attr("cx", d => this.X(d.bin)+bw*Math.random())
	    .attr("cy", d => this.Y(d.value))
	    .attr("r", d => 2)
    }
   
    else if (options.kind == 'stairs') {
	g.selectAll("line")
	    .data(data)
	    .enter().append("line").attr('class','stairs')
	    .attr("x1", d => this.X(d.bin)-0*bw/2)
	    .attr("x2", d => this.X(d.bin)+2*bw/2)
	    .attr("y1", d => this.Y(d.value))
	    .attr("y2", d => this.Y(d.value))
    }
    
    return this
}

Graphics.prototype.discretePlot = function (f,opts) {
    var defaults = {
	'class' : 'discretePlot',
	'points' : true,
	'joined' : false,
	'filled' : false,
	'needles' : true,
	'pointsize' : 3,
	'parent' : null,
	'step' : 1
    }
    var options = Object.assign(defaults, opts || {})
    if (options['joined'])
	return this.plot(x => f(floor(x)))
    data = range(this.xRange()[0],this.xRange()[1],options.step)
	.map(x => [x,f(x)])
    return this.listPlot(data, options)
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
    var x1,x2
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
    if (options['opacity'])
    	g.attr("opacity",options['opacity'])
    g.attr("class",options['class'])
    return this
}

Graphics.prototype.disc = function ([x,y],r,opts)
{
    var defaults = {
	'class'  : null,
	'parent' : null
        }

    var options = Object.assign(defaults, opts || {})    

    var parent = options.parent && d3.select(options.parent) || this.paper
    var g = this.paper.append('circle')
	.attr("cx",this.X(x))
	.attr("cy",this.Y(y))
	.attr("r",this.X(r))
    if (options['class'])
	g.attr("class",options['class'])
    return this
}

Graphics.prototype.line = function (pts,opts)
{
    var defaults = {
	'class'  : null,
	'parent' : null,
	'joined':true,
	'points':false
        }

    var options = Object.assign(defaults, opts || {})    

    return this.listPlot(pts,options)
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

Graphics.prototype.matrixPlot = function (m,opts)
{
    var defaults = {
	'class'  : null,
	'parent' : null
        }

    var options = Object.assign(defaults, opts || {})    
    var parent = options.parent && d3.select(options.parent) || this.paper
    
    if (this.xRange() == 'automatic')
	this.xRange([1,m[0].length+1])
    if (this.yRange() == 'automatic')
	this.yRange([1,m.length+1])
    var g = this.paper.append("g").attr('class','matrixPlot')
    pmax = m.map(r => r.max()).max()
    m.reversed().forEach((r,y) => r.forEach((c,x) => 
    {
	if (c != 0)
	    g.append("rect")
	    .attr("x", this.X(x+1))
	    .attr("y", this.Y(y+2))
	    .attr("width", this.X(x+1) - this.X(x))
	    .attr("height", this.Y(y-1) - this.Y(y))
	    .attr('class', c < 0 ? 'negatives' : 'positives')
	    .attr('opacity', sqrt(Math.abs(c)/pmax))
    }))
    return this
}


const sups = ["⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹"]

function sup(n)
{
    if (n < 0) return "⁻"+sup(-n)
    if (n < 10) return sups[n]
    if (n >= 10) return Array.from((n).toString()).sum(sup,"")
}
