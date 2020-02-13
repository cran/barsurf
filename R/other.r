#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.dbl = function (x)
{	if (length (x) == 1)
		c (x, x)
	else
		x
}

.trpl = function (x)
{	if (length (x) == 1)
		c (x, x, x)
	else
		x
}

.intseq = function (n, a, b)
{	x = seq (a, b, length.out=n)
	as.integer (x)
}

.outer.3 = function (f, n, x, y, z)
{	fv = array (0, n)
	for (i in 1:n [1])
	{	for (j in 1:n [2])
		{	for (k in 1:n [3])
				fv [i, j, k] = f (x [i], y [j], z [k])
		}
	}
	fv
}

.outer.xy = function (x, y, f)
{	nx = length (x)
	ny = length (y)
	x = rep (x, times=ny)
	y = rep (y, each=nx)
	if (! missing (f) )
	{	v = f (x, y)
		x = v [[1]]
		y = v [[2]]
	}
	x = matrix (x, nx, ny)
	y = matrix (y, nx, ny)
	list (fx=x, fy=y)
}

#deprecated functions
.plot = function (f, x, y, fv, ..., d3=FALSE, contrast, xat, yat, xlabs, ylabs)
{	if (inherits (fv, "P.nmatrix") )
		fv = fv$x
	if (d3)
	{	with.labs = (! missing (xat) || ! missing (yat) || ! missing (xlabs) || ! missing (ylabs) )
		if (with.labs)
			f (x, y, fv, ..., arrows=FALSE, xat=xat, yat=yat, xlabs=xlabs, ylabs=ylabs)
		else
			f (x, y, fv, ...)
	}
	else
		f (x, y, fv, ..., xyrel="s")
}
.use.theme = function (theme="blue") set.bs.theme (theme)
use.theme = function (...) .use.theme (...)
plot2d.cell = function (...) .plot (plot_dfield, ...)
plot3d.bar = function (...) .plot (plot_bar, ..., d3=TRUE)
plot2d.contour = function (...) .plot (plot_cfield, ...)
plot3d.surface = function (...) .plot (plot_surface, ..., d3=TRUE)
plot2d.tricontour = function (...) .plot (plot_tricontour, ...)
plot3d.trisurface = function (...) .plot (plot_trisurface, ..., d3=TRUE)
.temp.par = function (..., zlim, contrast) par (...)

#functional versions
.plotf_dfield = function (f, g, xlim, ylim, ...)
{	x = xlim [1]:xlim [2]
	y = ylim [1]:ylim [2]
	fv = outer (x, y, g)
	f (x, y, fv, ...)
}
.plotf_cfield = function (f, g, xlim, ylim, n, ...)
{	n = .dbl (n)
	x = seq (xlim [1], xlim [2], length.out = n [1])
	y = seq (ylim [1], ylim [2], length.out = n [2])
	fv = outer (x, y, g)
	f (x, y, fv, ...)
}
.plotf_tricontour = function (f, g, n, ...)
{	x = seq (0, 1, length.out=n)
	fv = matrix (NA, n, n)
	for (i in 1:n)
	{	for (j in 1:(1 + n - i) )
		{	z = 1 - x [i] - x [j]
			if (z < 0)
				z = 0
			fv [i, j] = g (x [i], x [j], z)
		}
	}
	f (x, x, fv, ...)
}

plotf_dfield = function (f, xlim, ylim=xlim, ...) .plotf_dfield (plot_dfield, f, xlim, ylim, ...)
plotf_bar = function (f, xlim, ylim=xlim, zlim, ...) .plotf_dfield (plot_bar, f, xlim, ylim, ..., zlim=zlim)
plotf_cfield = function (f, xlim, ylim=xlim, ..., n=30) .plotf_cfield (plot_cfield, f, xlim, ylim, n, ...)
plotf_surface = function (f, xlim, ylim=xlim, zlim, ..., n=30) .plotf_cfield (plot_surface, f, xlim, ylim, n, ..., zlim=zlim)
plotf_tricontour = function (f, ..., n=30)
	.plotf_tricontour (plot_tricontour, f, n, ...)
plotf_trisurface = function (f, ..., n=30)
	.plotf_tricontour (plot_trisurface, f, n, ...)

.plotf_contour_3d = function (f, xlim, ylim, zlim, fb, base.contours, n, ..., ncontours=2)
{	if (! missing (fb) )
		ncontours = length (fb)
	if (ncontours == 0)
		stop ("ncontours == 0")
	if (is.list (n) )
	{	N = length (n)
		if (N != ncontours)
			stop ("n list but length (n) != ncontours")
		for (i in 1:N)
			n [[i]] = .trpl (n [[i]])
	}
	else if (is.vector (n) )
		n = rep (list (.trpl (n) ), ncontours)
	else
		stop ("n needs to be vector or list")
	x = y = z = fv = vector ("list", ncontours)
	for (i in 1:ncontours)
	{	x [[i]] = seq (xlim [1], xlim [2], length.out = n [[i]][1])
		y [[i]] = seq (ylim [1], ylim [2], length.out = n [[i]][2])
		z [[i]] = seq (zlim [1], zlim [2], length.out = n [[i]][3])
		fv [[i]] = .outer.3 (f, n [[i]], x [[i]], y [[i]], z [[i]])
	}
	if (base.contours)
	{	bx = seq (xlim [1], xlim [2], length.out=40)
		by = seq (ylim [1], ylim [2], length.out=40)
		bz = seq (zlim [1], zlim [2], length.out=20)
		fv.2 = .outer.3 (f, c (40, 40, 20), bx, by, bz)
		fv.2 = apply (fv.2, 1:2, min)
	
		if (missing (fb) )
		{	N = ncontours + 2
			flim = range (fv, na.rm=TRUE)
			fb = seq (flim [1], flim [2], length.out=N)[- c (1, N)]
		}
	
		bl = contourLines (bx, by, fv.2,, fb)
		for (i in 1:length (bl) )
			bl [[i]] = cbind (bl [[i]]$x, bl [[i]]$y)
	}
	else
		bl = NULL
	.plot_contour_3d (x, y, z, fv, xlim=xlim, ylim=ylim, zlim=zlim, ncontours=ncontours, fb=fb, base.lines=bl, ...)
}

.nested_isosurfaces = function (f, xlim, ylim, zlim, fb, base.contours, n, m, ..., ncontours=2)
{	if (! missing (fb) )
		ncontours = length (fb)
	if (ncontours < 2)
		stop ("needs >= 2 contours")
	n = .trpl (n)
	m = .trpl (m)
	nx = .intseq (ncontours, n [1], m [1])
	ny = .intseq (ncontours, n [2], m [2])
	nz = .intseq (ncontours, n [3], m [3])
	n = vector ("list", ncontours)
	for (i in 1:ncontours)
		n [[i]] = c (nx [i], ny [i], nz [i])
	.plotf_contour_3d (f, xlim, ylim, zlim, fb, base.contours, n, ..., ncontours=ncontours)
}

plotf_contour_3d = function (f, xlim, ylim=xlim, zlim=xlim, fb, ..., base.contours=TRUE, n=20)
	.plotf_contour_3d (f, xlim, ylim, zlim, fb, base.contours, n, ...)

nested_isosurfaces = function (f, xlim, ylim=xlim, zlim=xlim, fb, ..., base.contours=TRUE, nfirst=30, nlast=15)
	.nested_isosurfaces (f, xlim, ylim, zlim, fb, base.contours, nfirst, nlast, ...)

plotf_cfield_3d = function (f, xlim, ylim=xlim, zlim=xlim, ..., nslides=6, n=30, z)
{	n = .dbl (n)
	x = seq (xlim [1], xlim [2], length.out = n [1])
	y = seq (ylim [1], ylim [2], length.out = n [2])
	if (missing (z) )
		z = seq (zlim [1], zlim [2], length.out = nslides)
	else
		nslides = length (z)
	fv = vector ("list", nslides)
	for (i in 1:nslides)
		fv [[i]] = outer (x, y, f, z [i])
	plot_cfield_3d (x, y, z, fv, ...)
}

plotf_vecfield = function (f, xlim, ylim=xlim, ..., nv=20, nh=40)
	.plotf_vecfield (f, xlim, ylim, ..., nv=nv, nh=nh)

.plotf_vecfield = function (f, xlim, ylim, ..., vectors=TRUE, heatmap=TRUE, add=FALSE, nv, nh)
{	nv = .dbl (nv)
	nh = .dbl (nh)
	if (heatmap)
	{	x = seq (xlim [1], xlim [2], length.out = nh [1])
		y = seq (ylim [1], ylim [2], length.out = nh [2])
		fv = .outer.xy (x, y, f)
		plot_vecfield (x, y, fv$fx, fv$fy, vectors=FALSE, ...)
	}
	if (vectors)
	{	x = seq (xlim [1], xlim [2], length.out = nv [1])
		y = seq (ylim [1], ylim [2], length.out = nv [2])
		fv = .outer.xy (x, y, f)
		plot_vecfield (x, y, fv$fx, fv$fy, add=heatmap, heatmap=FALSE, ...)
	}
}

rotated.sinc = function (x, y)
{	r = sqrt (x^2 + y^2)
	fv = sin (r) / r
	fv [is.nan (fv)] = 1
	fv
}

bispherical.dist = function (x, y, z)
{	r1 = sqrt ( (x + 1)^2 + (y - 1)^2 + z^2)
	r2 = sqrt ( (x - 1)^2 + (y + 1)^2 + z^2)
	I = r2 < r1
	r1 [I] = r2 [I]
	r1
}

concentric.field = function (x, y)
{	theta = atan2 (x, y) - pi / 2
	r = 1 - abs (1 - sqrt (x^2 + y^2) )
	r [r < 0] = 0
	x = r * sin (theta)
	y = r * cos (theta)
	list (x, y)
}
