#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

plot_vec2 = function (x, y, dx, dy, ...,
	main="", xlab="x", ylab="y",
	vectors=TRUE, heatmap=TRUE,
	include.outermost.vectors=FALSE,
	add=FALSE, axes=TRUE, reverse=FALSE, swap.sides=FALSE,
	xyrel, xat, yat, xlabs, ylabs,
	arrowh.length=1.75, arrowh.width = 0.75 * arrowh.length,
	arrow.color="#000000", fill.color="#08080810",
	colf, colff, hcv=FALSE)
{	axes = .dbl (axes)
	reverse = .dbl (reverse)
	swap.sides = .dbl (swap.sides)
	if (missing (xyrel) )
		xyrel = test.xyrel (x, y, dx)

	nx = ny = NULL
	.UNPACK (.val.vec2 (x, y, dx, dy) )

	if (! add)
		p0 = .plot.new (main, xlab, ylab, xyrel, reverse, swap.sides, x, y)

	if (heatmap)
	{	gv = sqrt (dx ^ 2 + dy ^ 2)

		w = matrix (0, nrow=nx - 1, ncol=ny - 1)
		for (i in 1:(nx - 1) )
		{	for (j in 1:(ny - 1) )
			{	gsub = c (gv [i, j], gv [i, j + 1], gv [i + 1, j], gv [i + 1, j + 1])
				w [i, j] = mean (gsub)
			}
		}
		if (hcv)
			colf = .ST (colf, colff, w, TRUE)
		else
			colf = .ST (colf, colff, w, NULL, "flow")
		colors = colf (w)
		.plot.heatmap (nx - 1, ny - 1, x, y, colors)
	}
	if (vectors)
	{	if (include.outermost.vectors)
			.plot.vecs.2d (nx, ny, dx, dy, x, y, arrowh.length, arrowh.width, arrow.color, fill.color)
		else
		{	nx1 = nx - 1
			ny1 = ny - 1
			.plot.vecs.2d (nx - 2, ny - 2, dx [2:nx1, 2:ny1], dy [2:nx1, 2:ny1], x [2:nx1], y [2:ny1],
				arrowh.length, arrowh.width, arrow.color, fill.color)
		}
	}

	if (! add)
	{	.box (axes, reverse, xat, yat, xlabs, ylabs, swap.sides)
		par (p0)
	}
	.catchargs (...)
}

plot_vec3 = function (x, y, z, dx, dy, dz, ..., P = 1 / 3,
	main="", xlab="x", ylab="y", zlab="z",
	include.outermost.vectors=FALSE,
	axes=TRUE, z.axis = all (axes),
	ref.arrows = opt.ref.arrows (), z.ref.arrow = any (ref.arrows),
	xat, yat, xlabs, ylabs,
	nhl = opt.nhl (),
	head.color="#0000B0", mid.color="#8080FF", tail.color="#B0B0B0")
{	nx = ny = nz = NULL

	axes = .dbl (axes)
	ref.arrows = .dbl (ref.arrows)

	.UNPACK (.val.vec3 (x, y, z, dx, dy, dz) )
	zlim = range (z)

	if (axes [1] && ! ref.arrows [1]) .UNPACK (.axis.val (x, xat, xlabs) )
	if (axes [2] && ! ref.arrows [2]) .UNPACK (.axis.val (y, yat, ylabs) )
	.UNPACK (.axis.scale (x, xat) )
	.UNPACK (.axis.scale (y, yat) )
	.UNPACK (.axis.scale (z, 0) )

	p0 = .mar3 (nhl)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && ref.arrows [1], axes [2] && ref.arrows [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! ref.arrows [1]) .barsurf.xlabs (xat, xlabs)
	if (axes [2] && ! ref.arrows [2]) .barsurf.ylabs (yat, ylabs)
	if (z.axis) .z.axis (zlab, zlim, z.ref.arrow)

	if (include.outermost.vectors)
		.plot.vecs.3d (P, nx, ny, nz, dx, dy, dz, x, y, z, head.color, mid.color, tail.color)
	else
	{	nx1 = nx - 1
		ny1 = ny - 1
		nz1 = nz - 1
		.plot.vecs.3d (P, nx - 2, ny - 2, nz - 2,
			dx [2:nx1, 2:ny1, 2:nz1], dy [2:nx1, 2:ny1, 2:nz1], dz [2:nx1, 2:ny1, 2:nz1],
			x [2:nx1], y [2:ny1], z [2:nz1], head.color, mid.color, tail.color)
	}
	par (p0)
	.catchargs (...)
}

.plot.vecs.2d = function (nx, ny, dx, dy, x, y, arrowh.length, arrowh.width, arrow.color, fill.color)
{	line.width = .fine.line.width ()

	xstep = quantile (abs (diff (x) ), 0.75)
	ystep = quantile (abs (diff (y) ), 0.75)
	xm = max (abs (dx), na.rm=TRUE)
	ym = max (abs (dy), na.rm=TRUE)

	#center points
	xc = outer (x, rep (1, ny) )
	yc = outer (rep (1, nx), y)
	#standardized half dists
	hx = (xstep / xm * dx) / 2.1
	hy = (ystep / ym * dy) / 2.1

	.arrows (xc - hx, yc - hy, xc + hx, yc + hy, line.width, arrowh.length, arrowh.width, arrow.color, fill.color)
}

.plot.vecs.3d = function (P, nx, ny, nz, dx, dy, dz, x, y, z, head.color, mid.color, tail.color)
{	line.width = .fine.line.width ()

	xstep = quantile (abs (diff (x) ), 0.75)
	ystep = quantile (abs (diff (y) ), 0.75)
	zstep = quantile (abs (diff (z) ), 0.75)
	xm = max (abs (dx), na.rm=TRUE)
	ym = max (abs (dy), na.rm=TRUE)
	zm = max (abs (dz), na.rm=TRUE)
	
	h0 = w = array (0, c (nx, ny, nz) )
	I = array (FALSE, c (nx, ny, nz) )

	w [] = rank (sqrt (dx^2 + dy^2 + dz^2), FALSE) / (nx * ny * nz)
	I [] = (w > 1 - P)

	if (xm == 0) hx = h0
	else hx = (xstep / xm * dx)
	if (ym == 0) hy = h0
	else hy = (ystep / ym * dy)
	if (zm == 0) hz = h0
	else hz = (zstep / zm * dz)

	for (k in 1:nz)
	{	for (i in nx:1)
		{	for (j in ny:1)
			{	if (I [i, j, k])
				{	.compact.arrow (x [i] - hx [i, j, k], y [j] - hy [i, j, k], z [k] - hz [i, j, k],
						x [i] + hx [i, j, k], y [j] + hy [i, j, k], z [k] + hz [i, j, k],
						line.width, head.color, mid.color, tail.color)
				}
			}
		}
	}
}

.compact.arrow = function (x0, y0, z0, x1, y1, z1, lwd, head.color, mid.color, tail.color)
{	k = c (x0, y0, z0, x1, y1, z1)
	if (all (is.finite (k) ) )
	{	xc1 = 0.67 * x0 + 0.33 * x1
		yc1 = 0.67 * y0 + 0.33 * y1
		zc1 = 0.67 * z0 + 0.33 * z1
		xc2 = 0.33 * x0 + 0.67 * x1
		yc2 = 0.33 * y0 + 0.67 * y1
		zc2 = 0.33 * z0 + 0.67 * z1
		c1 = .project (xc1, yc1, zc1)
		c2 = .project (xc2, yc2, zc2)
		p0 = .project (x0, y0, z0)
		p1 = .project (x1, y1, z1)
		lines (c (p0 [1], c1 [1]), c (p0 [2], c1 [2]), lwd = 1 * lwd, col=tail.color)
		lines (c (c1 [1], c2 [1]), c (c1 [2], c2 [2]), lwd = 2 * lwd, col=mid.color)
		lines (c (c2 [1], p1 [1]), c (c2 [2], p1 [2]), lwd = 2 * lwd, col=head.color)
	}
}

.arrows = function (x0, y0, x1, y1, lwd=1, length=1.75, width=1.25, col="#000000", fill=col)
{	k = 0.0394
	xd = abs (xinch (k) )
	yd = abs (yinch (k) )
	ps.st = rbind (length * c (0, -1, -1, -1), width * c (0, 0.5, -0.5, 0) )
	for (i in seq_len (length (x0) ) )
	{	pxd = x1 [i] - x0 [i]
		pyd = y1 [i] - y0 [i]
		if (! is.na (pxd + pyd) )
		{	if (abs (pxd) > 1.25 * xd || abs (pyd) > 1.25 * yd)
			{	theta = atan2 (pyd / yd, pxd / xd)
				trans = matrix (c (cos (theta), sin (theta), -sin (theta), cos (theta) ), 2, 2)
				ps.rot = trans %*% ps.st
				ps.rot [1,] = xd * ps.rot [1,]
				ps.rot [2,] = yd * ps.rot [2,]
				ahps = ps.rot [,-4]
				alps = ps.rot [,4]

				lines (c (x0 [i], x1 [i] + alps [1]), c (y0 [i], y1 [i] + alps [2]), lwd=lwd, col=col)
				polygon (x1 [i] + ahps [1,], y1 [i] + ahps [2,], lwd=lwd, border=col, col=fill)
			}
			else if (abs (pxd) > 0.5 * xd || abs (pyd) > 0.5 * yd)
				lines (c (x0 [i], x1 [i]), c (y0 [i], y1 [i]), lwd=lwd, col=col)
		}
	}
}
