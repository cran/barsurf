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
	as.integer (round (x) )
}

.outer.3 = function (f, n, x, y, z)
{	x2 = rep (x, times = n [2] * n [3])
	y2 = rep (y, each = n [1], times = n [3])
	z2 = rep (z, each = n [1] * n [2])
	fv = f (x2, y2, z2)
	array (fv, n)
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

matrix.margins = function ()
{	mar = par ("mar")
	par (mar = mar [c (3, 2, 1, 4) ])
}
		
#deprecated functions
.plot = function (f, x, y, fv, ..., contrast, d3=FALSE)
{	if (d3) f (x, y, fv, ...)
	else f (x, y, fv, ..., xyrel="s")
}
.use.theme = function (theme="blue") set.bs.theme (theme)
use.theme = function (...) .use.theme (...)
plot2d.contour = function (...) .plot (plot_cfield, ...)
plot3d.bar = function (...) .plot (plot_bar, ..., d3=TRUE)
plot3d.surface = function (...) .plot (plot_surface, ..., d3=TRUE)
litmus.rainbow.fit = function (...) rainbow.litmus.fit (...)
.extract.cols = function (..., cols)
{	if (missing (cols) )
		NULL
	else
		cols
}

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

.panel.contours = function (x, y, fv, fb)
{	cl = contourLines (x, y, fv,, fb)
	ncl = length (cl)
	pc = vector ("list", ncl)
	for (i in seq_len (ncl) )
		pc [[i]] = cbind (cl [[i]]$x, cl [[i]]$y)
	pc
}

.plotf_contour_3d = function (f, xlim, ylim, zlim, n, ...,
	base.contours=FALSE, rear.contours=FALSE, pconstants, maximal=FALSE,
	ncontours=2, fb)
{	if (missing (fb) )
	{	if (ncontours == 0)
			stop ("ncontours == 0")
	}
	else
		ncontours = length (fb)
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
	if (missing (fb) )
	{	N = ncontours + 2
		flim = range (fv, na.rm=TRUE)
		fb = seq (flim [1], flim [2], length.out=N)[-c (1, N)]
		if (maximal)
			fb = rev (fb)
	}
	if (base.contours || rear.contours)
	{	if (missing (pconstants) )
		{	pconstants = numeric (3)
			pconstants [1] = mean (xlim)
			pconstants [2] = mean (ylim)
			pconstants [3] = mean (zlim)
		}
		else if (length (pconstants) != 3)
			stop ("length (pconstants) != 3")

		pc = vector ("list", 3)
		px = seq (min (xlim), max (xlim), length.out=40)
		py = seq (min (ylim), max (ylim), length.out=40)
		pz = seq (min (zlim), max (zlim), length.out=40)
		if (base.contours)
		{	fv.xy = outer (px, py, f, pconstants [3])
			pc [[3]] = .panel.contours (px, py, fv.xy, fb)
		}
		if (rear.contours)
		{	f.yz = function (y, z, f, x) f (x, y, z)
			fv.yz = outer (py, pz, f.yz, f, pconstants [1])
			pc [[1]] = .panel.contours (py, pz, fv.yz, fb)

			f.xz = function (x, z, f, y) f (x, y, z)
			fv.xz = outer (px, pz, f.xz, f, pconstants [2])
			pc [[2]] = .panel.contours (px, pz, fv.xz, fb)
		}
	}
	else
		pc = NULL
	.plot_contour_3d (x, y, z, fv, xlim=xlim, ylim=ylim, zlim=zlim, panel.lines=pc, fb=fb, ...)
}

.nested_isosurfaces = function (f, xlim, ylim, zlim, nfirst, nlast, ..., ncontours=2, fb)
{	if (missing (fb) )
	{	if (ncontours == 0)
			stop ("ncontours == 0")
	}
	else
		ncontours = length (fb)
	nfirst = .trpl (nfirst)
	nlast = .trpl (nlast)
	nx = .intseq (ncontours, nfirst [1], nlast [1])
	ny = .intseq (ncontours, nfirst [2], nlast [2])
	nz = .intseq (ncontours, nfirst [3], nlast [3])
	n = vector ("list", ncontours)
	for (i in 1:ncontours)
		n [[i]] = c (nx [i], ny [i], nz [i])
	.plotf_contour_3d (f, xlim, ylim, zlim, n, ..., ncontours=ncontours, fb=fb)
}

plotf_contour_3d = function (f, xlim, ylim=xlim, zlim=xlim, ..., n=20, maximal=FALSE,
	base.contours=FALSE, rear.contours=FALSE, pconstants)
{	.plotf_contour_3d (f, xlim, ylim, zlim, n, ...,
		base.contours=base.contours, rear.contours=rear.contours, pconstants=pconstants, maximal=maximal)
}

nested_isosurfaces = function (f, xlim, ylim=xlim, zlim=xlim, ..., nfirst=30, nlast=15)
	.nested_isosurfaces (f, xlim, ylim, zlim, nfirst, nlast, ...)

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
	.plot_cfield_3d.rev (x, y, z, fv, ..., zrng = diff (zlim) )
}

.plot_cfield_3d.rev = function (x, y, z, fv, ..., zrng, reverse.z)
{	if (missing (reverse.z) )
		reverse.z = (zrng < 0)
	plot_cfield_3d (x, y, z, fv, ..., reverse.z=reverse.z)
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
