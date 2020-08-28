#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.plotf_xfield = function (f, g, xlim, ylim, ..., .cont=FALSE, .n)
{	if (.cont)
	{	n = .dbl (.n)
		x = seq (xlim [1], xlim [2], length.out = n [1])
		y = seq (ylim [1], ylim [2], length.out = n [2])
	}
	else
	{	x = xlim [1]:xlim [2]
		y = ylim [1]:ylim [2]
	}
	fv = outer (x, y, g)
	f (x, y, fv, ..., reverse = .is.reverse (xlim, ylim) )
}

.plotf_cfield = function (g, xlim, ylim, ..., add=FALSE, contours=TRUE, heatmap=TRUE, nc, nh)
{	nc = .dbl (nc)
	nh = .dbl (nh)
	if (all (nc == nh) )
	{	x = seq (xlim [1], xlim [2], length.out = nc [1])
		y = seq (ylim [1], ylim [2], length.out = nc [2])
		fv = outer (x, y, g)
		plot_cfield (x, y, fv, ..., add=add, contours=contours, heatmap=heatmap,
			reverse = .is.reverse (xlim, ylim) )
	}
	else
	{	if (heatmap)
		{	x = seq (xlim [1], xlim [2], length.out = nh [1])
			y = seq (ylim [1], ylim [2], length.out = nh [2])
			fv = outer (x, y, g)
			plot_cfield (x, y, fv, ..., add=add, contours=FALSE,
				reverse = .is.reverse (xlim, ylim) )
		}
		if (contours)
		{	x = seq (xlim [1], xlim [2], length.out = nc [1])
			y = seq (ylim [1], ylim [2], length.out = nc [2])
			fv = outer (x, y, g)
			plot_cfield (x, y, fv, ..., add=heatmap, heatmap=FALSE,
				reverse = .is.reverse (xlim, ylim) )
		}
	}
}

.plotf_tricontour = function (f, g, n, ...)
{	x = seq (0, 1, length.out=n)
	fv = matrix (NA, n, n)
	for (i in 1:n)
	{	for (j in 1:(1 + n - i) )
			fv [i, j] = g (x [i], x [j])
	}
	f (x, x, fv, ...)
}

.panel.contours = function (x, y, fv, fb)
{	cl = contourLines (x, y, fv,, fb)
	ncl = length (cl)
	pc = vector ("list", ncl)
	for (i in seq_len (ncl) )
		pc [[i]] = cbind (cl [[i]]$x, cl [[i]]$y)
	pc
}

plotf_dfield = function (f, xlim, ylim=xlim, ...)
	.plotf_xfield (plot_dfield, f, xlim, ylim, ...)
plotf_bar = function (f, xlim, ylim=xlim, zlim, ...)
	.plotf_xfield (plot_bar, f, xlim, ylim, ..., zlim=zlim)
plotf_cfield = function (f, xlim, ylim=xlim, ..., n=30, nc = max (n, 60), nh=n)
	.plotf_cfield (f, xlim, ylim, nc=nc, nh=nh, ...)
plotf_surface = function (f, xlim, ylim=xlim, zlim, ..., n=30)
	.plotf_xfield (plot_surface, f, xlim, ylim, ..., .cont=TRUE, .n=n, zlim=zlim)
plotf_tricontour = function (f, ..., n=30)
	.plotf_tricontour (plot_tricontour, f, n, ...)
plotf_trisurface = function (f, ..., n=30)
	.plotf_tricontour (plot_trisurface, f, n, ...)

plotf_isosurface = function (f, xlim, ylim=xlim, zlim=xlim, ...,
	nsurfaces=2, fb, fq,
	nested=TRUE, maximal,
	panel.contours=TRUE,
	base.contours=panel.contours, rear.contours=panel.contours, pconstants,
	n)
{	nsurfaces = .nsurfaces (nsurfaces, fb, fq)

	if (missing (n) )
	{	n = vector ("list", nsurfaces)
		if (nested && nsurfaces > 1)
			k = .intseq (nsurfaces, 30, 15)
		else
			k = rep (20, nsurfaces)
		for (i in 1:nsurfaces)
			n [[i]] = rep (k [i], 3)
	}
	else
	{	if (is.list (n) )
		{	N = length (n)
			if (N != nsurfaces)
				stop ("n list but length (n) != ncontours")
			for (i in 1:N)
				n [[i]] = .trpl (n [[i]])
		}
		else if (is.vector (n) )
			n = rep (list (.trpl (n) ), nsurfaces)
		else
			stop ("n needs to be vector or list")
	}

	x = y = z = fv = vector ("list", nsurfaces)
	for (i in 1:nsurfaces)
	{	x [[i]] = seq (xlim [1], xlim [2], length.out = n [[i]][1])
		y [[i]] = seq (ylim [1], ylim [2], length.out = n [[i]][2])
		z [[i]] = seq (zlim [1], zlim [2], length.out = n [[i]][3])
		fv [[i]] = .outer.3 (f, n [[i]], x [[i]], y [[i]], z [[i]])
	}
	fb = .init.contours (nsurfaces, fb, fq, fv [[1]], nested, maximal)

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
		{	fv.xy = outer (px, py, f, rep (pconstants [3], 1600) )
			pc [[3]] = .panel.contours (px, py, fv.xy, fb)
		}
		if (rear.contours)
		{	f.yz = function (y, z, f, x) f (x, y, z)
			fv.yz = outer (py, pz, f.yz, f, rep (pconstants [1], 1600) )
			pc [[1]] = .panel.contours (py, pz, fv.yz, fb)

			f.xz = function (x, z, f, y) f (x, y, z)
			fv.xz = outer (px, pz, f.xz, f, rep (pconstants [2], 1600) )
			pc [[2]] = .panel.contours (px, pz, fv.xz, fb)
		}
	}
	else
		pc = NULL
	reverse = .is.reverse (xlim, ylim)
	z.reverse = .is.reverse (zlim)[1]
	plot_isosurface (x, y, z, fv, fb=fb, reverse=reverse, z.reverse=z.reverse, ..., .panel.lines=pc)
}

plotf_cfield3 = function (f, xlim, ylim=xlim, zlim=xlim, ..., nslides=6, z.reverse=FALSE, z, n=30)
{	n = .dbl (n)
	x = seq (xlim [1], xlim [2], length.out = n [1])
	y = seq (ylim [1], ylim [2], length.out = n [2])
	if (missing (z) )
	{	z = seq (zlim [1], zlim [2], length.out = nslides)
		z = signif (z, 3)
		z.reverse = .is.reverse (zlim)[1]
	}	
	else
		nslides = length (z)
	fv = vector ("list", nslides)
	for (i in 1:nslides)
		fv [[i]] = outer (x, y, f, z [i])
	plot_cfield3 (x, y, z, fv, ..., reverse = .is.reverse (xlim, ylim), z.reverse=z.reverse)
}

.plotf_vec2 = function (f, xlim, ylim, ..., add=FALSE, vectors=TRUE, heatmap=TRUE, nv, nh)
{	nv = .dbl (nv)
	nh = .dbl (nh)
	if (all (nv == nh) )
	{	x = seq (xlim [1], xlim [2], length.out = nv [1])
		y = seq (ylim [1], ylim [2], length.out = nv [2])
		fv = .outer.dxdy (f, nv, x, y)
		plot_vec2 (x, y, fv [[1]], fv [[2]], add=add, vectors=vectors, heatmap=heatmap, ...,
			reverse = .is.reverse (xlim, ylim) )
	}
	else
	{	if (heatmap)
		{	x = seq (xlim [1], xlim [2], length.out = nh [1])
			y = seq (ylim [1], ylim [2], length.out = nh [2])
			fv = .outer.dxdy (f, nh, x, y)
			plot_vec2 (x, y, fv [[1]], fv [[2]], add=add, vectors=FALSE, ...,
				reverse = .is.reverse (xlim, ylim) )
		}
		if (vectors)
		{	x = seq (xlim [1], xlim [2], length.out = nv [1])
			y = seq (ylim [1], ylim [2], length.out = nv [2])
			fv = .outer.dxdy (f, nv, x, y)
			plot_vec2 (x, y, fv [[1]], fv [[2]], add=heatmap, heatmap=FALSE, ...,
				reverse = .is.reverse (xlim, ylim) )
		}
	}
}

plotf_vec2 = function (vf, xlim, ylim=xlim, ..., n=20, nv=n, nh = max (n, 40) )
	.plotf_vec2 (vf, xlim, ylim, ..., nv=nv, nh=nh)

plotf_vec3 = function (vf, xlim, ylim=xlim, zlim=xlim, ..., n = c (20, 22, 16) )
{	n = .trpl (n)
	if (diff (xlim) <= 0 || diff (ylim) <= 0 || diff (ylim) <= 0 )
		stop ("plotf_vec3 needs ascending xlim/ylim/zlim")
	x = seq (xlim [1], xlim [2], length.out = n [1])
	y = seq (ylim [1], ylim [2], length.out = n [2])
	z = seq (zlim [1], zlim [2], length.out = n [3])
	fv = .outer.dz (vf, n, x, y, z)
	plot_vec3 (x, y, z, fv [[1]], fv [[2]], fv [[3]], ...)
}
