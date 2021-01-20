#barsurf: Contour Plots, 3D Plots, Vector Fields and Heatmaps
#Copyright (C), Abby Spurdle, 2018 to 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.nsurfaces = function (nsurfaces, fb, fq)
{	if (missing (fb) )
	{	if (missing (fq) )
			n = as.integer (nsurfaces)
		else
			n = length (as.numeric (fq) )
	}
	else
		n = length (as.numeric (fb) )
	if (n == 0)
		stop ("nsurfaces == 0")
	n
}

.init.contours =  function (nsurfaces, fb, fq, fv, nested, maximal)
{	if (missing (fb) )
	{	if (missing (fq) )
		{	if (nested)
			{	if (nsurfaces == 1) fq = 0.05
				else if (nsurfaces == 2) fq = c (0.025, 0.1275)
				else if (nsurfaces == 3) fq = c (0.0125, 0.075, 0.275)
				else stop ("fb or fq needed, if nsurfaces > 3")
	
				if (missing (maximal) )
					maximal = .is.maximal (fv)
				if (maximal)
					fq = 1 - fq
			}
			else
			{	if (nsurfaces == 1) fq = 0.5
				else if (nsurfaces == 2) fq = c (0.45, 0.55)
				else if (nsurfaces == 3) fq = c (0.4, 0.5, 0.6)
				else stop ("fb or fq needed, if nsurfaces > 3")
			}
		}
		else
			fq = as.numeric (fq)
		fb = as.vector (quantile (unlist (fv), fq, na.rm=TRUE) )
		
	}
	else
		fb = as.numeric (fb)
	fb
}

plot_isosurface = function (x, y, z, fv, ..., fb, fq,
	main="", xlab="x", ylab="y", zlab="z",
	wire.frame=FALSE, texture=TRUE,
	axes=TRUE, z.axis = all (axes),
	ref.arrows = opt.ref.arrows (), z.ref.arrow = any (ref.arrows),
	reverse=FALSE, z.reverse=FALSE,
	nsurfaces=2,
	nested=TRUE, maximal,
	xat, yat, xlabs, ylabs,
	nhl = opt.nhl (),
	wire.color="#808080", iso.colors = st.iso.colors (theme), theme)
{	n = 0

	axes = .dbl (axes)
	ref.arrows = .dbl (ref.arrows)
	reverse = .dbl (reverse)

	nsurfaces = .nsurfaces (nsurfaces, fb, fq)
	iso.colors = rep_len (iso.colors, nsurfaces)

	.UNPACK (.iso.val (x, y, z, fv, nsurfaces) )
	xlim = .val.xlim (nsurfaces, x, reverse [1])
	ylim = .val.xlim (nsurfaces, y, reverse [2])
	zlim = .val.xlim (nsurfaces, z, z.reverse)

	if (axes [1] && ! ref.arrows [1]) .UNPACK (.axis.val (xlim, xat, xlabs, xname="x") )
	if (axes [2] && ! ref.arrows [2]) .UNPACK (.axis.val (ylim, yat, ylabs, xname="y") )
	.UNPACK (.axis.scale.iso (x, xat, nsurfaces, xlim) )
	.UNPACK (.axis.scale.iso (y, yat, nsurfaces, ylim) )
	.UNPACK (.axis.scale.iso (z, 0, nsurfaces, zlim) )

	fb = .init.contours (nsurfaces, fb, fq, fv [[1]], nested, maximal)

	P = vector ("list", nsurfaces)
	nP = integer (nsurfaces)
	for (i in 1:nsurfaces)
	{	P [[i]] = misc3d::computeContour3d (fv [[i]], max (fv [[i]]), fb [i], x [[i]], y [[i]], z [[i]])
		nP [i] = as.integer (round (nrow (P [[i]]) / 3) )
	}

	I = 1
	N = sum (nP)
	if (N == 0)
		stop ("no isosurfaces detected")
	depth.table = matrix (0L, N, 3)
	for (i in 1:nsurfaces)
	{	for (j in seq_len (nP [i]) )
		{	depth.table [I, 1] = i
			depth.table [I, 2] = j
			J = 3 * (j - 1) + 1
			depth.table [I, 3] = .compute.depth (P [[i]][J:(J + 2),])
			I = I + 1
		}
	}
	K = order (depth.table [,3], decreasing=TRUE)
	depth.table = depth.table [K,]

	p0 = .mar3 (nhl)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && ref.arrows [1], axes [2] && ref.arrows [2], reverse [1], reverse [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! ref.arrows [1]) .barsurf.xlabs (xat, xlabs)
	if (axes [2] && ! ref.arrows [2]) .barsurf.ylabs (yat, ylabs)
	if (z.axis) .z.axis (zlab, zlim, z.ref.arrow)

	line.width = .fine.line.width ()
	if (! wire.frame)
		wire.color = NA
	colors = character (N)
	if (texture)
	{	for (i in 1:nsurfaces)
		{	I = (depth.table [,1] == i)
			colors [I] = .randomize.color (iso.colors [i], nP [i], nsurfaces)
		}
	}
	else
	{	for (i in 1:nsurfaces)
		{	I = (depth.table [,1] == i)
			colors [I] = iso.colors [i]
		}
	}

	panel.lines = .extract.private.args (...)$panel.lines
	if (! is.null (panel.lines) )
	{	for (lines in panel.lines [[1]])
		{	py = (lines [,1] - ylim [1]) / diff (ylim)
			pz = (lines [,2] - zlim [1]) / diff (zlim)
			.barsurf.lines (1, py, pz, "#B0B0B0")
		}
		for (lines in panel.lines [[2]])
		{	px = (lines [,1] - xlim [1]) / diff (xlim)
			pz = (lines [,2] - zlim [1]) / diff (zlim)
			.barsurf.lines (px, 1, pz, "#B0B0B0")
		}
		for (lines in panel.lines [[3]])
		{	px = (lines [,1] - xlim [1]) / diff (xlim)
			py = (lines [,2] - ylim [1]) / diff (ylim)
			.barsurf.lines (px, py, 0, "#B0B0B0")
		}
	}

	for (i in 1:N)
	{	I = depth.table [i, 1]
		K = depth.table [i, 2]
		J = 3 * (K - 1) + 1
		J = J:(J + 2)
		xsub = P [[I]][J, 1]
		ysub = P [[I]][J, 2]
		zsub = P [[I]][J, 3]
		.barsurf.poly (xsub, ysub, zsub, colors [i], line.width, wire.color)
	}
	par (p0)
	.catchargs (...)
}

plot_cfield3 = function (x, y, z, fv, ..., fb,
	main="", xlab="x", ylab="y", zlab="z",
	contours=TRUE, heatmap=TRUE,
	axes=TRUE, ref.arrows = opt.ref.arrows (),
	slide.labels = any (axes),
	reverse=FALSE, z.reverse=FALSE,
	ncontours=6, emph="n",
	xat, yat, xlabs, ylabs, zlabs,
	nhl = opt.nhl (),
	colf, colff, theme)
{	axes = .dbl (axes)
	reverse = .dbl (reverse)
	ref.arrows = .dbl (ref.arrows)

	nx = ny = nz = ztype = 0
	.UNPACK (.val.cf3d (x, y, z, fv, reverse) )

	if (axes [1] && ! ref.arrows [1]) .UNPACK (.axis.val (x, xat, xlabs) )
	if (axes [2] && ! ref.arrows [2]) .UNPACK (.axis.val (y, yat, ylabs) )
	.UNPACK (.axis.scale (x, xat, reverse [1]) )
	.UNPACK (.axis.scale (y, yat, reverse [2]) )

	if (slide.labels)
	{	if (missing (zlabs) )
			zlabs = signif (z, 3)
		else if (length (zlabs) == nz)
			zlabs = zlabs [I]
		else
			stop ("length (zlabs) != number of slides")
	}
	else
		zlabs = character (nz)

	.UNPACK (.axis.scale (z) )

	if (ztype != "+")
	{	I = order (z)
		fv = fv [I]
		z = z [I]
		zlabs = zlabs [I]
	}
	if (z.reverse)
	{	fv = rev (fv)
		z = 1 - rev (z)
		zlabs = rev (zlabs)
	}

	flim = range (unlist (fv), na.rm=TRUE)
	if (missing (fb) )
	{	if (contours)
		{	N = ncontours + 2
			fb = seq (flim [1], flim [2], length.out=N)[-c (1, N)]
		}
	}
	else
		ncontours = length (fv)

	p0 = .mar3 (nhl)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && ref.arrows [1], axes [2] && ref.arrows [2], reverse [1], reverse [2], TRUE)
	title (main)
	.barsurf.labs (xlab, ylab, TRUE)
	if (axes [1] && ! ref.arrows [1]) .barsurf.xlabs (xat, xlabs, TRUE)
	if (axes [2] && ! ref.arrows [2]) .barsurf.ylabs (yat, ylabs, TRUE)

	if (heatmap)
	{	alpha = .alpha.glass (emph)

		w = vector ("list", nz)
		for (k in 1:nz)
		{	w [[k]] = matrix (0, nrow=nx - 1, ncol=ny - 1)
			for (i in 1:(nx - 1) )
			{	for (j in 1:(ny - 1) )
				{	fsub = c (fv [[k]][i, j], fv [[k]][i, j + 1], fv [[k]][i + 1, j], fv [[k]][i + 1, j + 1])
					w [[k]][i, j] = mean (fsub)
				}
			}
		}
		color.function = .ST (colf, colff, unlist (w), theme, NULL, "glass", alpha=alpha)
	}
	for (k in 1:nz)
	{	if (heatmap)
		{	colors = color.function (w [[k]])
			.plot.heatmap.2 (nx - 1, ny - 1, x, y, z [k], colors)
		}
		if (contours && ncontours > 0)
		{	v = contourLines (x, y, fv [[k]],, fb)
			for (p in v)
				.barsurf.lines.2 (p$x, p$y, z [k], "#00000080")
		}
		.barsurf.poly.2 (c (0, 0, 1, 1), c (0, 1, 1, 0), z [k], "#00000080", NA)
		xy = .project.2 (0, 1, z [k])
		if (slide.labels)
			text (xy [1], xy [2] + 0.05, zlabs [k], adj = c (0, 0.5) )
	}
	xy = .project.2 (0, 1, 1)
	text (xy [1], xy [2] + 0.15, zlab, adj = c (0, 0.5) )
	par (p0)
	.catchargs (...)
}

.compute.depth = function (P)
{	P = apply (P, 2, mean)
	x = (P [1] - P [2]) / 2
	sqrt ( (P [1] - x)^2 + (P [2] + x)^2)
}

.randomize.color = function (col, n=1, ncontours)
{	x = as.vector (col2rgb (col, TRUE) )
	if (ncontours == 1) err = 25
	else if (ncontours == 2) err = 50
	else err = 87.5
	r = .rc.ext (x [1], n, err)
	g = .rc.ext (x [2], n, err)
	b = .rc.ext (x [3], n, err)
	a = .rc.ext (x [4], n, 25)
	rgb (r, g, b, a, maxColorValue=255)
}

.rc.ext = function (x, n, err=110)
{	x = x + runif (n, -err, err)
	x [x < 0] = 0
	x [x > 255] = 255
	x
}

.alpha.glass = function (emph)
{	if (emph == "n")
		alpha = 0.3
	else if (emph == "b")
	{	k = seq (-1, 1, length.out=10)
		alpha = k ^ 2
	}
	else if (emph == "l")
	{	k = seq (-1, 0, length.out=10)
		alpha = k ^ 2
	}
	else if (emph == "h")
	{	k = seq (0, 1, length.out=10)
		alpha = k ^ 2
	}
	else if (emph == "B")
	{	k = seq (-1, 1, length.out=10)
		alpha = k ^ 4
	}
	else if (emph == "L")
	{	k = seq (-1, 0, length.out=10)
		alpha = k ^ 4
	}
	else if (emph == "H")
	{	k = seq (0, 1, length.out=10)
		alpha = k ^ 4
	}
	else
		stop ("emph not in {n, b, l, h, B, L, H}")
	alpha
}

.plot.heatmap.2 = function (nr, nc, x, y, z, colors)
{	for (i in 1:nr)
	{	for (j in 1:nc)
		{	xsub = c (x [i], x [i], x [i + 1], x [i + 1])
			ysub = c (y [j], y [j + 1], y [j + 1], y [j])
	    		.barsurf.poly.2 (xsub, ysub, c (z, z, z, z), NA, colors [i, j])
		}
	}
}
