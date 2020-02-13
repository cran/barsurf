#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.barsurf.frame = function (arrow.x=TRUE, arrow.y=TRUE)
{	.barsurf.poly (c (0, 0, 1, 1), c (0, 1, 1, 0), 0, rgb (0.925, 0.925, 0.925) )
	p1 = .project (0, 0, 0)
	p2 = .project (1, 0, 0)
	p3 = .project (0, 1, 0)
	d = xinch (0.05)
	if (arrow.x)
		.arrows (p1 [1] + d, p1 [2] - d, p2 [1] + d, p2 [2] - d)
	if (arrow.y)
		.arrows (p1 [1] - d, p1 [2] - d, p3 [1] - d, p3 [2] - d)

	.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0), rgb (0.975, 0.975, 0.975) )
	.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0), rgb (0.975, 0.975, 0.975) )
}

.barsurf.plane.xy.2 = function (arrow.x=TRUE, arrow.y=TRUE)
{	p1 = .project.2 (0, 0, 0)
	p2 = .project.2 (1, 0, 0)
	p3 = .project.2 (0, 1, 0)
	d = xinch (0.05)
	if (arrow.x)
		.arrows (p1 [1] + d, p1 [2] - d, p2 [1] + d, p2 [2] - d)
	if (arrow.y)
		.arrows (p1 [1] - d, p1 [2] - d, p3 [1] - d, p3 [2] - d)
}

.barsurf.labs = function (xlab, ylab)
{	x = c (0.525, -0.525)
	y = 0.16
	text (x, y, c (xlab, ylab) )
}

.barsurf.labs.2 = function (xlab, ylab)
{	x = c (0.525, -0.525)
	y = 0.06
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
{	.barsurf.vline (c (x2, x2), c (y2, y2), c (0, h) )
	.barsurf.poly (c (x1, x1, x2, x2), y1, c (0, h, h, 0), col2)
	.barsurf.poly (x1, c (y1, y1, y2, y2), c (0, h, h, 0), col2)
	.barsurf.poly (c (x1, x1, x2, x2), c (y1, y2, y2, y1), h, col1)
}

.barsurf.vline = function (x, y, z)
{	ps = .project (x, y, z)
	lines (ps [,1], ps [,2])
}

.barsurf.poly = function (x, y, z, col="white", lwd=1, border="black")
{	ps = .project (x, y, z)
	polygon (ps [,1], ps [,2], lwd=lwd, border=border, col=col)
}

.barsurf.poly.2 = function (x, y, z, border, col)
{	ps = .project.2 (x, y, z)
	polygon (ps [,1], ps [,2], border=border, col=col)
}

.barsurf.lines = function (x, y, z, col)
{	ps = .project (x, y, z)
	lines (ps [,1], ps [,2], col=col)
}

.barsurf.lines.2 = function (x, y, z, col)
{	ps = .project.2 (x, y, z)
	lines (ps [,1], ps [,2], col=col)
}

.project = function (x, y, z)
{	x2 = x * cos (pi / 4) + y * cos (pi * 3 / 4)
	y2 = x * sin (pi / 4) + y * sin (pi * 3 / 4)
	y3 = 0.7 * y2 + 0.5 * z
	cbind (x=x2, y=y3)
}

.project.2 = function (x, y, z)
{	x2 = x * cos (pi / 4) + y * cos (pi * 3 / 4)
	y2 = x * sin (pi / 4) + y * sin (pi * 3 / 4)
	y3 = 0.05 + 0.2 * y2 + 1.1 * z
	cbind (x=x2, y=y3)
}
