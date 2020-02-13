#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.axis.val.3d = function (with.xlabs, label, nx, rnames, xat, xlabs, x)
{	xaxis = .axis.val ("x", nx, rnames, xat, xlabs, x, with.xlabs)
	xb = xaxis [[3]]
	xlim = range (xb)
	xb = (xb - xlim [1]) / diff (xlim)
	if (with.xlabs)
	{	if (missing (xat) )
			xat = xaxis [[1]]
		xat = (xat - xlim [1]) / diff (xlim)
		list (xat, xaxis [[2]], xb)
	}
	else
		list (0, 0, xb)
}

plot_bar = function (x, y, fv, ...,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	zlim, axes=TRUE, arrows=TRUE, color.function, cols)
{	nx = nrow (fv)
	ny = ncol (fv)
	axes = .dbl (axes)
	arrows = .dbl (arrows)
	with.xlabs = (axes [1] && ! arrows [1])
	with.ylabs = (axes [2] && ! arrows [2])
	xaxis = .axis.val.3d (with.xlabs, "x", nx, rownames (fv), xat, xlabs, x)
	yaxis = .axis.val.3d (with.ylabs, "y", ny, colnames (fv), yat, ylabs, y)
	xb = xaxis [[3]]
	yb = yaxis [[3]]
	if (missing (zlim) )
		zlim = range (fv, na.rm=TRUE)
	dfv = diff (zlim)
	if (dfv == 0)
		fv [] = 0.5
	else
	    fv = (fv - zlim [1]) / dfv
	if (missing (main) )
		main = ""

	p0 = par (mar=c (1, 0.2, 1, 0.2) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && arrows [1], axes [2] && arrows [2])
	title (main)
	.barsurf.labs (xlab, ylab)

	if (with.xlabs)
		.barsurf.xlabs (xaxis [[1]], substring (xaxis [[2]], 1, 6) )
	if (with.ylabs)
		.barsurf.ylabs (yaxis [[1]], substring (yaxis [[2]], 1, 6) )

	if (missing (cols) )
	{	if (missing (color.function) )
		{	bf = getOption ("barsurf")$barface
			bf = eval (str2lang (bf) )
			color.function = bf ()
		}
		cols.t = matrix (color.function (TRUE), nx, ny)
		cols.f = matrix (color.function (FALSE), nx, ny)
	}
	else
	{	if (is.matrix (cols) )
			cols.t = cols.f = cols
		else
		{	cols.t = cols [[1]]
			cols.f = cols [[2]]
		}
	}

	for (i in nx:1)
	{	for (j in ny:1)
    		{	if (is.na (fv [i, j]) )
				NULL
			else
				.barsurf.bar (xb [i], xb [i + 1], yb [j], yb [j + 1], fv [i, j], cols.t [i, j], cols.f [i, j])
		}
	}
    par (p0)
}

plot_surface = function (x, y, fv, ...,
	grid.lines=TRUE,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	zlim, axes=TRUE, arrows=TRUE, grid.col, color.function, color.fit)
{	nx = nrow (fv)
	ny = ncol (fv)
	axes = .dbl (axes)
	arrows = .dbl (arrows)
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
	xs = (x - min (x) ) / diff (range (x) )
	ys = (y - min (y) ) / diff (range (y) )
	if (missing (main) )
		main = ""

	w = matrix (0, nrow = nx - 1, ncol = ny - 1)
	if (.is.const (zlim, fv) )
		fv [] = 0.5
	else
	{	if (missing (zlim) )
			zlim = range (fv)
		fv = (fv - zlim [1]) / diff (zlim)
		for (i in 1:(nx - 1) )
		{	for (j in 1:(ny - 1) )
			{	fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
				w [i, j] = max (fsub) - min (fsub)
			}
		}
		if (.is.plane (w) )
			w [] = 0
	}

	p0 = par (mar=c (1, 0.2, 1, 0.2) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && arrows [1], axes [2] && arrows [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! arrows [1])
		.surface.axes ("x", x, xat, xlabs)
	if (axes [2] && ! arrows [2])
		.surface.axes ("y", y, yat, ylabs)

	v = .line.attr (grid.lines, grid.col)
	if (missing (color.function) )
	{	if (missing (color.fit) )
		{	color.fit = getOption ("barsurf")$litmus.fit.lum
			color.fit = eval (str2lang (color.fit) )
		}
		color.function = color.fit (w)
	}
	cols = color.function (w)
	for (i in (nx - 1):1)
	{	for (j in (ny - 1):1)
		{	xsub = c (xs [i], xs [i], xs [i + 1], xs [i + 1])
			ysub = c (ys [j], ys [j + 1], ys [j + 1], ys [j])
			fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j + 1], fv [i + 1, j])
			.barsurf.poly (xsub, ysub, fsub, cols [i, j], v [[1]], v [[2]])
		}
	}
	par (p0)
}

plot_trisurface = function (x, y, fv, ...,
	grid.lines=TRUE,
	main, xlab="x", ylab="y",
	zlim, axes=TRUE, arrows=TRUE, grid.col, color.function, color.fit)
{	n = .test.fv (fv)
	axes = .dbl (axes)
	arrows = .dbl (arrows)
	x = y = seq (0, 1, length.out=n)
	if (missing (main) )
		main = ""

	w1 = w2 = matrix (0, nrow = n - 1, ncol = n - 1)
	if (.is.const (zlim, fv) )
		fv [] = 0.5
	else
	{	if (missing (zlim) )
			zlim = range (fv, na.rm=TRUE)
		fv = (fv - zlim [1]) / diff (zlim)
		for (i in 1:(n - 1) )
		{	for (j in 1:(n - i) )
			{	fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j])
				w1 [i, j] = max (fsub) - min (fsub)
			}
		}
		for (i in 1:(n - 2) )
		{	if (i < n - 1)
			{	for (j in 1:(n - i - 1) )
				{	fsub = c (fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
					w2 [i, j] = max (fsub) - min (fsub)
				}
			}
		}
		if (.is.plane (c (w1, w2) ) )
		{	w1 [] = 0
			w2 [] = 0
		}
	}

	p0 = .temp.par (mar=c (1, 0.2, 1, 0.2) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && arrows [1], axes [2] && arrows [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! arrows [1])
		.trisurface.axes ("x", x)
	if (axes [2] && ! arrows [2])
		.trisurface.axes ("y", y)

	v = .line.attr (grid.lines, grid.col)
	if (missing (color.function) )
	{	if (missing (color.fit) )
		{	color.fit = getOption ("barsurf")$litmus.fit.lum
			color.fit = eval (str2lang (color.fit) )
		}
		color.function = color.fit (c (w1, w2) )
	}
	cols1 = color.function (w1)
	cols2 = color.function (w2)
	for (i in (n - 1):1)
	{	for (j in (n - i):1)
		{	if (i < n - 1 && j < n - i)
			{	xsub = c (x [i], x [i + 1], x [i + 1])
				ysub = c (y [j + 1], y [j], y [j + 1])
				fsub = c (fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
				colstr = cols1 [i, j]
				.barsurf.poly (xsub, ysub, fsub, colstr, v [[1]], v [[2]])
			}	
			xsub = c (x [i], x [i], x [i + 1])
			ysub = c (y [j], y [j + 1], y [j])
			fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j])
			colstr = cols1 [i, j]
			.barsurf.poly (xsub, ysub, fsub, colstr, v [[1]], v [[2]])
		}
	}
	par (p0)
}

.is.const = function (flim, fv)
{	u = unique.default (fv)
	if (length (u) == 1)
		TRUE
	else if (missing (flim) )
		FALSE
	else if (diff (range (fv, na.rm=TRUE) ) < diff (flim) / 1000)
		TRUE
	else
		FALSE
}

.is.plane = function (w)
{	meanw = abs (mean (w, na.rm=TRUE) )
	dw = diff (range (w, na.rm=TRUE) )
	(dw < abs (meanw) / 1000)
}

.line.attr = function (grid.lines, grid.color, iso=FALSE)
{	rs = getOption ("barsurf")$rendering.style
	line.width = 1
	if (grid.lines)
	{	if (missing (grid.color) )
		{	if (rs == "p")
				grid.color = "#000000"
			else if (iso)
				grid.color = "#404040"
			else
				grid.color = getOption ("barsurf")$soft.line.col
		}
		if (rs == "e")
			line.width = 0.125
	}
	else
		grid.color = NA
	list (line.width, grid.color)
}

.surface.axes = function (which, x, xat, xlabs)
{	xlim = range (x)
	xrng = diff (xlim)
	if (missing (xat) )
	{	if (! missing (xlabs) )
			stop ("xat/yat needed if xlabs/ylabs supplied")
		xat = pretty (x)
		n = length (xat)
		if (n > 2)
		{	dx = xrng / 15
			xat = xat [xat > xlim [1] + dx & xat < xlim [2] - dx]
		}
	}
	if (missing (xlabs) )
		xlabs = xat
	xat = (xat - min (x) ) / xrng
	if (which == "x")
		.barsurf.xlabs (xat, xlabs)
	else
		.barsurf.ylabs (xat, xlabs)
}

.trisurface.axes = function (which, x)
{	xlim = range (x)
	xrng = diff (xlim)
	xat = pretty (x)
	n = length (xat)
	if (n > 2)
	{	dx = xrng / 15
		xat = xat [xat > xlim [1] + dx & xat < xlim [2] - dx]
	}
	xlabs = xat
	xat = (xat - min (x) ) / xrng
	if (which == "x")
		.barsurf.xlabs (xat, xlabs)
	else
		.barsurf.ylabs (xat, xlabs)
}
