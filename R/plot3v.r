#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.iso.var = function (label, I, ncontours, fv, x, is.array)
{	n = integer (ncontours)
	for (i in 1:ncontours)
		n [i] = dim (fv [[i]])[I]
	if (missing (x) )
	{	if (is.array)
			x = 1:(n [1])
		else
			stop ("x, y and z required if fv list")
		rep (list (x), ncontours)
	}
	else
	{	if (is.list (x) )
		{	N = length (x)
			if (N == ncontours)
			{	for (i in 1:ncontours)
				{	if (is.vector (x [[i]]) )
					{	if (length (x [[i]]) != n [i])
							stop (sprintf ("length (%s [[%s]]) != dim (fv [[%i]])[%s]", label, I, i, I) )
					}
					else
						stop ("x, y and z need to be vectors or lists of vectors")
				}
			}
			else
				stop (sprintf ("if %s list, but length (%s) != ncontours", label, label) )		
		}
		else if (is.vector (x) )
		{	if (is.array)
			{	if (length (x) != n [1])
					stop (sprintf ("length (%s) != dim (fv)[%s]", label, I) )
			}
			else
			{	for (i in 1:ncontours)
				{	if (length (x) != n [i])
						stop (sprintf ("length (%s) != dim (fv [[%s]])[%s]", label, i, I) )
				}
			}
			x = rep (list (x), ncontours)

		}
		else
			stop ("x, y and z need to be vectors or lists of vectors")
	}
	x
}

.plot_contour_3d = function (x, y, z, fv, fb, ...,
	wire.frame=FALSE,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	xlim, ylim, zlim,
	axes=TRUE, arrows=TRUE,
	base.lines,
	ncontours=2, wire.frame.col="#808080", iso.cols)
{	if (missing (fb) )
	{	if (ncontours == 0)
			stop ("ncontours == 0")
		N = ncontours + 2
		flim = range (fv, na.rm=TRUE)
		fb = seq (flim [1], flim [2], length.out=N)[-c (1, N)]
	}
	else
		ncontours = length (fb)

	is.array = FALSE
	if (is.array (fv) )
	{	is.array = TRUE
		.val.iso.array (fv)
		fv = rep (list (fv), ncontours)
	}
	else if (is.list (fv) )
	{	if (ncontours != length (fv) )
			stop ("fv list, but length (fv) != ncontours")
		for (fg in fv)
			.val.iso.array (fg)
	}
	else
		stop ("fv needs to be array or list of arrays")
	x = .iso.var ("x", 1, ncontours, fv, x, is.array)
	y = .iso.var ("y", 2, ncontours, fv, y, is.array)
	z = .iso.var ("z", 3, ncontours, fv, z, is.array)

	if (ncontours == 1)
		primary = 1
	else if (ncontours == 2)
	{	s1 = sum (dim (fv [[1]]) )
		s2 = sum (dim (fv [[2]]) )
		primary = which.max (c (s1, s2) )
	}
	else
		primary = ceiling (ncontours / 2)

	xlim = .val.xlim (xlim, ncontours, x)
	ylim = .val.xlim (ylim, ncontours, y)
	zlim = .val.xlim (zlim, ncontours, z)
	xs = ys = zs = vector ("list", ncontours)
	for (i in 1:ncontours)
	{	xs [[i]] = (x [[i]] - xlim [1]) / diff (xlim)
		ys [[i]] = (y [[i]] - ylim [1]) / diff (ylim)
		zs [[i]] = (z [[i]] - zlim [1]) / diff (zlim)
	}

	P = vector ("list", ncontours)
	nP = integer (ncontours)
	for (i in 1:ncontours)
	{	P [[i]] = misc3d::computeContour3d (fv [[i]], max (fv [[i]]), fb [i], xs [[i]], ys [[i]], zs [[i]])
		nP [i] = as.integer (round (nrow (P [[i]]) / 3) )
	}
	I = 1
	N = sum (nP)
	if (N == 0)
		stop ("no isosurfaces detected")
	depth.table = matrix (0L, N, 3)
	for (i in 1:ncontours)
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

	axes = .dbl (axes)
	arrows = .dbl (arrows)
	if (missing (main) )
		main = ""

	p0 = par (mar=c (1, 0.2, 1, 0.2) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && arrows [1], axes [2] && arrows [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! arrows [1])
		.surface.axes ("x", xlim, xat, xlabs)
	if (axes [2] && ! arrows [2])
		.surface.axes ("y", ylim, yat, ylabs)

	v = .line.attr (wire.frame, wire.frame.col)
	lwd = v [[1]]
	wire.frame.col = v [[2]]
	if (missing (iso.cols) )
	{	iso.cols = c ("#DDA06060", "#0040FF20", "#80FF0016")
		if (ncontours == 1)
			iso.cols = iso.cols [2]
		else if (ncontours == 2 || ncontours ==3)
			iso.cols = iso.cols [1:ncontours]
		else
			stop ("for > 3 isosurfaces, please specify their colors")
	}
	cols = character (N)
	for (i in 1:ncontours)
	{	I = (depth.table [,1] == i)
		cols.sub = .randomize.color (iso.cols [i], nP [i])
		cols [I] = cols.sub
	}

	if (! missing (base.lines) && ! is.null (base.lines) )
	{	for (lines in base.lines)
		{	sbx = (lines [,1] - xlim [1]) / diff (xlim)
			sby = (lines [,2] - ylim [1]) / diff (ylim)
			.barsurf.lines (sbx, sby, 0, "#B0B0B0")
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
		.barsurf.poly (xsub, ysub, zsub, cols [i], lwd, wire.frame.col)
	}
	par (p0)
}

plot_contour_3d = function (x, y, z, fv, fb, ...,
	wire.frame=FALSE,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	xlim, ylim, zlim,
	axes=TRUE, arrows=TRUE,
	ncontours=2, wire.frame.col="#808080", iso.cols)
{	.plot_contour_3d (x, y, z, fv, fb, ...,
		wire.frame=wire.frame,
		main=main, xlab=xlab, ylab=ylab, xat=xat, yat=yat, xlabs=xlabs, ylabs=ylabs,
		xlim=xlim, ylim=ylim, zlim=zlim,
		axes=axes, arrows=arrows,
		ncontours=ncontours, wire.frame.col=wire.frame.col, iso.cols=iso.cols)
}

plot_cfield_3d = function (x, y, z, fv, fb, ...,
	contours=TRUE, heatmap=TRUE,
	main, xlab="x", ylab="y",
	axes=TRUE,
	ncontours=6, emph="n", color.function, color.fit)
{	nz = length (fv)
	if (nz < 2)
		stop ("need 2 or more slices")
	nx = nrow (fv [[1]])
	ny = ncol (fv [[2]])
	axes = .dbl (axes)
	for (i in 1:nz)
	{	if (nx != nrow (fv [[i]]) || ny != ncol (fv [[i]]) )
			stop ("all fv matrices need to be same size")
	}
	if (nx < 2 || ny < 2)
		stop ("nrow (fv) or ncol (fv) < 2")
	if (missing (x) )
		x = 1:nx
	else if (length (x) != nx)
		stop ("length (x) != nrow (fv)")
	if (missing (y) )
		y = 1:ny
	else if (length (y) != ny)
		stop ("length (y) != ncol (fv)")
	if (missing (z) )
		z = 1:nz
	else if (length (z) != nz)
		stop ("length (z) != length (fv)")
	x = (x - min (x) ) / diff (range (x) )
	y = (y - min (y) ) / diff (range (y) )
	z = (z - min (z) ) / diff (range (z) )
	flim = range (fv, na.rm=TRUE)
	if (missing (main) )
		main = ""

	if (missing (fb) )
	{	if (contours)
		{	N = ncontours + 2
			fb = seq (flim [1], flim [2], length.out=N)[-c (1, N)]
		}
	}
	else
		ncontours = length (fv)

	p0 = par (mar=c (1, 0.2, 1, 0.2) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.plane.xy.2 (axes [1], axes [2])
	title (main)
	.barsurf.labs.2 (xlab, ylab)

	if (heatmap)
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
		if (missing (color.function) )
		{	if (missing (color.fit) )
			{	color.fit = getOption ("barsurf")$litmus.fit.glass
				color.fit = eval (str2lang (color.fit) )
			}
			color.function = color.fit (unlist (w), alpha)
		}
	}
	for (k in 1:nz)
	{	if (heatmap)
		{	cols = color.function (w [[k]])
			.plot.heatmap.2 (nx - 1, ny - 1, x, y, z [k], cols)
		}
		if (contours && ncontours > 0)
		{	v = contourLines (x, y, fv [[k]],, fb)
			for (p in v)
				.barsurf.lines.2 (p$x, p$y, z [k], "#00000080")
		}
		.barsurf.poly.2 (c (0, 0, 1, 1), c (0, 1, 1, 0), z [k], "#00000080", NA)
	}
	par (p0)
}

.val.iso.array = function (fv)
{	if (is.array (fv) )
	{	if (any (dim (fv) < 6) )
			stop ("all dim (fv) of dim (fv [[i]]) need to be >= 6")
	}
	else
		stop ("fv needs to be array or list of arrays")
}

.val.xlim = function (xlim, ncontours, x)
{	if (missing (xlim) )
	{	xlim = matrix (0, ncontours, 2)
		for (i in 1:ncontours)
			xlim [i,] = range (x [[i]])
		s = xlim [,1] - xlim [,2]
		xlim = xlim [which.max (s),] 
	}
	xlim
}

.compute.depth = function (P)
{	P = apply (P, 2, mean)
	x = (P [1] - P [2]) / 2
	sqrt ( (P [1] - x)^2 + (P [2] + x)^2)
}

.plot.heatmap.2 = function (nr, nc, x, y, z, cols)
{	for (i in 1:nr)
	{	for (j in 1:nc)
		{	xsub = c (x [i], x [i], x [i + 1], x [i + 1])
			ysub = c (y [j], y [j + 1], y [j + 1], y [j])
	    		.barsurf.poly.2 (xsub, ysub, c (z, z, z, z), NA, cols [i, j])
		}
	}
}

.randomize.color = function (col, n=1)
{	x = as.vector (col2rgb (col, TRUE) )
	r = .rc.ext (x [1], n)
	g = .rc.ext (x [2], n)
	b = .rc.ext (x [3], n)
	a = .rc.ext (x [4], n, 25)
	rgb (r, g, b, a, maxColorValue=255)
}

.rc.ext = function (x, n, err=110)
{	x = x + runif (n, -err, err)
	x [x < 0] = 0
	x [x > 255] = 255
	x
}
