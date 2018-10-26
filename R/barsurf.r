plot3d.empty = function ()
{   p0 = par (mar=c (1.75, 0.175, 0.9, 0.175) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.plane.xy ()
	.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	.barsurf.labs ("x", "y")
	par (p0)
}

#length (x) = nrow (z) + 1
#length (y) = ncol (z) + 1
plot3d.bar = function (x, y, z,
	main, xlab="x", ylab="y", col1=rgb (0, 0.8, 0.4), col2=rgb (0.5, 1, 0.7),
	zlim)
{   p0 = par (mar=c (1, 0.2, 1, 0.2) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.plane.xy ()
	.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	if (!missing (main) )
		title (main)
	.barsurf.labs (xlab, ylab)
	if (missing (x) )
		x = 1:(nrow (z) + 1)
	if (missing (y) )
		y = 1:(ncol (z) + 1)
	nx = length (x) - 1
	ny = length (y) - 1
	x = (x - min (x) ) / diff (range (x) )
	y = (y - min (y) ) / diff (range (y) )
	if (missing (zlim) )
		zlim = range (z)
    d = diff (zlim)
    if (d == 0)
		z [TRUE] = 0
	else
	    z = (z - zlim [1]) / d
	if (length (col1) == 1)
		col1 = matrix (col1, nrow=nx, ncol=ny)
	if (length (col2) == 1)
		col2 = matrix (col2, nrow=nx, ncol=ny)
	for (i in nx:1)
	    for (j in ny:1)
    	{	x1 = x [i]
    		x2 = x [i + 1]
    		y1 = y [j]
    		y2 = y [j + 1]
    		h = z [i, j]
			.barsurf.bar (x1, x2, y1, y2, h, col1 [i, j], col2 [i, j])
	}
    par (p0)
}

#length (x) = nrow (z)
#length (y) = ncol (z)
plot3d.surf = function (x, y, z,
	main, xlab="x", ylab="y", col1=rgb (0, 0.8, 0.4), col2="white",
	zlim)
{	p0 = par (mar=c (1, 0.2, 1, 0.2) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.plane.xy ()
	.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	if (!missing (main) )
		title (main)
	.barsurf.labs (xlab, ylab)
	if (missing (x) )
		x = 1:nrow (z)
	if (missing (y) )
		y = 1:ncol (z)
	nx = length (x) - 1
	ny = length (y) - 1
	x = (x - min (x) ) / diff (range (x) )
	y = (y - min (y) ) / diff (range (y) )
	if (missing (zlim) )
		zlim = range (z)
    d = diff (zlim)
    if (d == 0)
		z [TRUE] = 0
	else
	    z = (z - zlim [1]) / d
	for (i in nx:1)
	    for (j in ny:1)
    	{	x1 = x [i]
    		x2 = x [i + 1]
    		y1 = y [j]
    		y2 = y [j + 1]
    		h = c (z [i, j], z [i, j + 1], z [i + 1, j + 1], z [i + 1, j])
    		t = (h [3] - h [1]) / sqrt ( (x2 - x1) ^ 2 + (y2 - y1) ^ 2)
    		if (t < 0)
    		    t = 0
    		w = 2 * atan (t) / pi
			w = sqrt (w)
    		.barsurf.poly (c (x1, x1, x2, x2), c (y1, y2, y2, y1), h, .int.col (col1, col2, w) )
	    }
	par (p0)
}

.barsurf.plane.xy = function ()
{	.barsurf.poly (c (0, 0, 1, 1), c (0, 1, 1, 0), 0)
	p1 = .project (0, 0, 0)
	p2 = .project (1, 0, 0)
	p3 = .project (0, 1, 0)
	d = 0.03
	arrows (p1 [1] + d, p1 [2] - d, p2 [1] + d, p2 [2] - d, length=0.12)
	arrows (p1 [1] - d, p1 [2] - d, p3 [1] - d, p3 [2] - d, length=0.12)
}

.barsurf.labs = function (xlab, ylab)
{	x = c (0.525, -0.525)
	y = 0.16
	text (x, y, c (xlab, ylab) )
}

.barsurf.bar = function (x1, x2, y1, y2, h, col1, col2)
{	.barsurf.poly (c (x1, x1, x2, x2), y1, c (0, h, h, 0), col2)
	.barsurf.poly (x1, c (y1, y1, y2, y2), c (0, h, h, 0), col2)
	.barsurf.poly (c (x1, x1, x2, x2), c (y1, y2, y2, y1), h, col1)
}

.barsurf.poly = function (x, y, z, col="white")
{	ps = .project (x, y, z)
	polygon (ps [,1], ps [,2], col=col)
}

.project = function (x, y, z)
{	x2 = x * cos (pi / 4) + y * cos (pi * 3 / 4)
	y2 = x * sin (pi / 4) + y * sin (pi * 3 / 4)
	y3 = 0.7 * y2 + 0.5 * z
	cbind (x=x2, y=y3)
}

.int.col = function (col1, col2, w)
{	col1 = col2rgb (col1) / 255
    col2 = col2rgb (col2) / 255
    col = col1 + w * (col2 - col1)
    col [col < 0] = 0
    col [col > 1] = 1
	rgb (col [1], col [2], col [3])
}
