.barsurf.plane.xy = function (arrow.x=TRUE, arrow.y=TRUE)
{	.barsurf.poly (c (0, 0, 1, 1), c (0, 1, 1, 0), 0)
	p1 = .project (0, 0, 0)
	p2 = .project (1, 0, 0)
	p3 = .project (0, 1, 0)
	d = 0.03
	if (arrow.x)
		arrows (p1 [1] + d, p1 [2] - d, p2 [1] + d, p2 [2] - d, length=0.12)
	if (arrow.y)
		arrows (p1 [1] - d, p1 [2] - d, p3 [1] - d, p3 [2] - d, length=0.12)
}

.barsurf.labs = function (xlab, ylab)
{	x = c (0.525, -0.525)
	y = 0.16
	text (x, y, c (xlab, ylab) )
}

.barsurf.xlabs = function (x, xlabs)
{	for (i in 1:length (x) )
	{	p = .project (x [i], -0.04, 0)
		text (p [1], p [2], xlabs [i], c (0, 0.5) )
	}
}

.barsurf.ylabs = function (y, ylabs)
{	for (i in 1:length (y) )
	{	p = .project (-0.04, y [i], 0)
		text (p [1], p [2], ylabs [i], c (1, 0.5) )
	}
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
