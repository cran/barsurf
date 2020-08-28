#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.barsurf.frame = function (arrow.x=TRUE, arrow.y=TRUE, rev.x=FALSE, rev.y=FALSE, cf3d=FALSE)
{	if (cf3d)
	{	p1 = .project.2 (0, 0, 0)
		p2 = .project.2 (1, 0, 0)
		p3 = .project.2 (0, 1, 0)
	}
	else
	{	.barsurf.poly (c (0, 0, 1, 1), c (0, 1, 1, 0), 0, rgb (0.925, 0.925, 0.925) )
		.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0), rgb (0.975, 0.975, 0.975) )
		.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0), rgb (0.975, 0.975, 0.975) )
		p1 = .project (0, 0, 0)
		p2 = .project (1, 0, 0)
		p3 = .project (0, 1, 0)
	}

	d = xinch (0.05)
	if (arrow.x)
	{	if (rev.x) .arrows (p2 [1] + d, p2 [2] - d, p1 [1] + d, p1 [2] - d)
		else .arrows (p1 [1] + d, p1 [2] - d, p2 [1] + d, p2 [2] - d)
	}
	if (arrow.y)
	{	if (rev.y) .arrows (p3 [1] - d, p3 [2] - d, p1 [1] - d, p1 [2] - d)
		else .arrows (p1 [1] - d, p1 [2] - d, p3 [1] - d, p3 [2] - d)
	}
}

.z.axis = function (zlab, zlim, ref.arrow=FALSE)
{	p = .project (0, 1, 1) + c (0.125, 0.35)
	if (nchar (zlab) <= 10)
		text (p [1] - 0.025, p [2], zlab, adj = c (1, 0.5) )
	else
		text (p [1], p [2], zlab)
	p = rbind (p, p)
	p [,2] = p [,2] + c (-0.14, 0.14)
	if (ref.arrow)
	{	if (zlim [1] > zlim [2]) .arrows (p [2, 1], p [2, 2], p [1, 1], p [1, 2])
		else .arrows (p [1, 1], p [1, 2], p [2, 1], p [2, 2])
	}
	else
	{	lines (p [,1], p [,2])
		lines (c (p [1, 1], p [1, 1] + 0.0125) , rep (p [1, 2], 2) )
		lines (c (p [2, 1], p [2, 1] + 0.0125) , rep (p [2, 2], 2) )
		text (p [,1] + 0.025, p [,2], trimws (format (zlim, digits=4) ), adj = c (0, 0.5) )
	}
}

.barsurf.labs = function (xlab, ylab, cf3d=FALSE)
{	if (cf3d) text (c (0.525, -0.525), 0.06, c (xlab, ylab) )
	else text (c (0.525, -0.525), 0.16, c (xlab, ylab) )

}

.barsurf.xlabs = function (x, xlabs, cf3d=FALSE)
{	for (i in 1:length (x) )
	{	if (cf3d) p = .project.2 (x [i], -0.1, 0)
		else p = .project (x [i], -0.04, 0)
		text (p [1], p [2], xlabs [i], c (0, 0.5) )
	}
}

.barsurf.ylabs = function (y, ylabs, cf3d=FALSE)
{	for (i in 1:length (y) )
	{	if (cf3d) p = .project.2 (-0.1, y [i], 0)
		else p = .project (-0.04, y [i], 0)
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
	y3 = 0.7 * y2 + 0.525 * z
	cbind (x=x2, y=y3)
}

.project.2 = function (x, y, z)
{	x2 = x * cos (pi / 4) + y * cos (pi * 3 / 4)
	y2 = x * sin (pi / 4) + y * sin (pi * 3 / 4)
	y3 = 0.05 + 0.2 * y2 + 1.1 * z
	cbind (x=x2, y=y3)
}
