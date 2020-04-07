function Canvas(id) {
    this.context = id.getContext('2d'),
    this.width = this.context.canvas.width,
    this.height = this.context.canvas.height,
    this.X = x => x,
    this.Y = x => x,
    this.Xp = x => x,
    this.scale = function (xmin,xmax,ymin,ymax)
    {
	this.scaleX(xmin,xmax)
	this.scaleY(ymin,ymax)
	return this
    },
    this.scaleX = function (xmin,xmax)
    {
	this.X = rescale(xmin,xmax,0,this.context.canvas.width)
	this.Xp = rescale(0,this.context.canvas.width,xmin,xmax)
	return this
    },
    this.scaleY = function (ymin,ymax)
    {
	this.Y = rescale(ymin,ymax,this.context.canvas.height,0)
	this.Yp = rescale(this.context.canvas.height,0,ymin,ymax)
return this
    },
    this.clear = function ()
    {
	this.context.clearRect(0,0,this.context.canvas.width,this.context.canvas.height)
	return this
    }
}

function rescale(a,b,c,d)
{
    return x => (x-a)/(b-a)*(d-c)+c
}

function listPlot(cnv,data)
{
    const ctx = cnv.context
    var x = data[0].length==1 ? i => i : i => data[i][0]  
    var y = data[0].length==1 ? i => data[i] : i => data[i][1]  
    ctx.beginPath()
    for(var i=0; i< data.length;i++)
    { 
	ctx.moveTo(cnv.X(x(i)),cnv.Y(y(i)))
	ctx.arc(cnv.X(x(i)),cnv.Y(y(i)),1,0,Math.PI*2)
    }
    ctx.stroke()
}

function listLinePlot(cnv,data)
{
    const ctx = cnv.context
    var x = data[0].length==1 ? i => i : i => data[i][0]  
    var y = data[0].length==1 ? i => data[i] : i => data[i][1]  
    ctx.beginPath()
    ctx.moveTo(cnv.X(x(0)),cnv.Y(y(0)))
    for(var i=1; i< data.length;i++)
	ctx.lineTo(cnv.X(x(i)),cnv.Y(y(i)))
    ctx.stroke()
}


function plot(cnv,f)
{
    const ctx = cnv.context
    ctx.beginPath()
    ctx.moveTo(cnv.Xp(0),cnv.Y(f(cnv.Xp(0))))
    for(var i=1; i< cnv.width;i++)
    {
	ctx.lineTo(i,cnv.Y(f(cnv.Xp(i))))
    }
    ctx.stroke()
}


function discretePlot(cnv,data,spec)
{
    var X = cnv.X, Y = cnv.Y
    if (spec == 'log')
    {
	Y = x => cnv.Y(Math.log(x))
    }
    if (spec == 'loglog')
    {
	Y = x => cnv.Y(Math.log(x))
	X = x => cnv.X(Math.log(x+1))
    }
    if (spec == 'loglin')
    {
	X = x => cnv.X(Math.log(x+1))
    }
    const ctx = cnv.context
    ctx.beginPath()
    for(var i=0; i< data.length;i++)
    {
	ctx.moveTo(X(i),Y(0))
	ctx.lineTo(X(i),Y(data[i]))
	ctx.arc(X(i),Y(data[i]),1,0,Math.PI*2)
    }
    ctx.stroke()
}

function disc(cnv,x,y,r)
{
    cnv.context.beginPath()
    cnv.context.arc(cnv.X(x),cnv.Y(y),r,0,Math.PI*2)
    cnv.context.fill()
   
}

function Xgrid(cnv,pts)
{
    cnv.context.strokeStyle = 'gray'
    cnv.context.globalAlpha = 0.5
    cnv.context.beginPath()
    pts.forEach(x => {
	cnv.context.moveTo(cnv.X(x),0)
	cnv.context.lineTo(cnv.X(x),cnv.height)
    })
    cnv.context.stroke()
    cnv.context.globalAlpha = 1
}

function Ygrid(cnv,pts)
{
    cnv.context.strokeStyle = 'gray'
    cnv.context.globalAlpha = 0.5
    cnv.context.beginPath()
    pts.forEach(y => {
	cnv.context.moveTo(0,cnv.Y(y))
	cnv.context.lineTo(cnv.width,cnv.Y(y))
    })
    cnv.context.stroke()
    cnv.context.globalAlpha = 1
}
