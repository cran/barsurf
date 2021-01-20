#barsurf: Contour Plots, 3D Plots, Vector Fields and Heatmaps
#Copyright (C), Abby Spurdle, 2018 to 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

plot_bar = function (x, y, fv, ...,
	main="", xlab="x", ylab="y", zlab="z",
	axes=TRUE, z.axis=FALSE,
	ref.arrows = opt.ref.arrows (), reverse=FALSE,
	continuous.axes=FALSE,
	zlim, xat, yat, xlabs, ylabs,
	nhl = opt.nhl (),
	top.color = st.top.color (theme), side.color = st.side.color (theme), theme)
{	args = .extract.private.args (...)
	if (! is.null (args$arrows) )
		ref.arrows = args$arrows

	axes = .dbl (axes)
	reverse = .dbl (reverse)
	ref.arrows = .dbl (ref.arrows)
	continuous.axes = .dbl (continuous.axes)

	nx = ny = xb = yb = 0
	.UNPACK (.val.xy (x, y, fv, TRUE, reverse=reverse) )

	if (axes [1] && ! ref.arrows [1]) .UNPACK (.axis.val (x, xat, xlabs, continuous.axes [1], names = rownames (fv) ) )
	if (axes [2] && ! ref.arrows [2]) .UNPACK (.axis.val (y, yat, ylabs, continuous.axes [2], names = colnames (fv) ) )
	.UNPACK (.axis.scale (xb, xat, reverse [1], nx) )
	.UNPACK (.axis.scale (yb, yat, reverse [2], ny) )

	if (missing (zlim) )
		zlim = range (fv, na.rm=TRUE)
	dfv = diff (zlim)
	if (dfv == 0) fv [] = 0.5
	else fv = (fv - zlim [1]) / dfv

	p0 = .mar3 (nhl)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && ref.arrows [1], axes [2] && ref.arrows [2], reverse [1], reverse [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! ref.arrows [1]) .barsurf.xlabs (xat, xlabs)
	if (axes [2] && ! ref.arrows [2]) .barsurf.ylabs (yat, ylabs)
	if (z.axis) .z.axis (zlab, zlim)

	args.cols = .extract.private.args (...)$cols
	if (is.null (args.cols) )
	{	if (length (top.color) == 1) top.color = matrix (top.color, nx, ny)
		if (length (side.color) == 1) side.color = matrix (side.color, nx, ny)
		if (! (is.matrix (top.color) && nrow (top.color) == nx && ncol (top.color) == ny ) )
			stop ("top.color not scalar or suitable matrix")
		if (! (is.matrix (side.color) && nrow (side.color) == nx && ncol (side.color) == ny ) )
			stop ("side.color not scalar or suitable matrix")
	}
	else
	{	if (is.matrix (args.cols) )
			top.color = side.color = args.cols
		else
		{	top.color = args.cols [[1]]
			side.color = args.cols [[2]]
		}
	}

	for (i in nx:1)
	{	for (j in ny:1)
    		{	if (is.na (fv [i, j]) )
				NULL
			else
				.barsurf.bar (xb [i], xb [i + 1], yb [j], yb [j + 1], fv [i, j], top.color [i, j], side.color [i, j])
		}
	}
	par (p0)
	.catchargs (...)
}

plot_surface = function (x, y, fv, ...,
	main="", xlab="x", ylab="y", zlab="z",
	grid=TRUE, gradient.shading=TRUE,
	axes=TRUE, z.axis=FALSE,
	ref.arrows = opt.ref.arrows (), reverse=FALSE,
	zlim, xat, yat, xlabs, ylabs,
	nhl = opt.nhl (),
	grid.color = st.sgrid.color (theme),
	colf, colff, theme)
{	args = .extract.private.args (...)
	if (! is.null (args$arrows) )
		ref.arrows = args$arrows

	axes = .dbl (axes)
	reverse = .dbl (reverse)
	ref.arrows = .dbl (ref.arrows)

	nx = ny = 0
	.UNPACK (.val.xy (x, y, fv, reverse=reverse) )
	fv0 = fv

	if (axes [1] && ! ref.arrows [1]) .UNPACK (.axis.val (x, xat, xlabs) )
	if (axes [2] && ! ref.arrows [2]) .UNPACK (.axis.val (y, yat, ylabs) )
	.UNPACK (.axis.scale (x, xat, reverse [1]) )
	.UNPACK (.axis.scale (y, yat, reverse [2]) )

	w = matrix (0, nrow = nx - 1, ncol = ny - 1)
	if (.is.const (zlim, fv) )
	{	if (missing (zlim) )
			fv [] = 0.5
		else
			fv = (fv - zlim [1]) / diff (zlim)
	}
	else
	{	if (missing (zlim) )
			zlim = range (fv)
		fv = (fv - zlim [1]) / diff (zlim)
		if (gradient.shading)
		{	for (i in 1:(nx - 1) )
			{	for (j in 1:(ny - 1) )
				{	fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
					w [i, j] = max (fsub) - min (fsub)
				}
			}
			if (.is.plane (w) )
				w [] = 0
		}
	}

	p0 = .mar3 (nhl)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && ref.arrows [1], axes [2] && ref.arrows [2], reverse [1], reverse [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! ref.arrows [1]) .barsurf.xlabs (xat, xlabs)
	if (axes [2] && ! ref.arrows [2]) .barsurf.ylabs (yat, ylabs)
	if (z.axis) .z.axis (zlab, zlim)

	line.width = .fine.line.width ()
	if (! grid)
		grid.color = NA
	if (gradient.shading)
	{	colf = .ST (colf, colff, w, theme, NULL, "lum")
		colors = colf (w)
	}
	else
	{	colf = .ST (colf, colff, fv0, theme, NULL, "lum")
		colors = colf (fv0)
	}
	for (i in (nx - 1):1)
	{	for (j in (ny - 1):1)
		{	xsub = c (x [i], x [i], x [i + 1], x [i + 1])
			ysub = c (y [j], y [j + 1], y [j + 1], y [j])
			fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j + 1], fv [i + 1, j])
			.barsurf.poly (xsub, ysub, fsub, colors [i, j], line.width, grid.color)
		}
	}
	par (p0)
	.catchargs (...)
}

plot_trisurface = function (x, y, fv, ...,
	main="", xlab="x", ylab="y", zlab="z",
	grid=TRUE,
	axes=TRUE, z.axis=FALSE,
	ref.arrows = opt.ref.arrows (),
	zlim,
	xat, yat, xlabs, ylabs,
	nhl = opt.nhl (),
	grid.color = st.sgrid.color (theme),
	colf, colff, theme)
{	axes = .dbl (axes)
	ref.arrows = .dbl (ref.arrows)

	n = 0
	.UNPACK (.val.tri (fv) )
	x = y = seq (0, 1, length.out=n)

	if (axes [1] && ! ref.arrows [1]) .UNPACK (.axis.val (x, xat, xlabs) )
	if (axes [2] && ! ref.arrows [2]) .UNPACK (.axis.val (y, yat, ylabs) )

	w1 = w2 = matrix (0, nrow = n - 1, ncol = n - 1)

	if (.is.const (zlim, fv) )
	{	if (missing (zlim) )
			fv [] = 0.5
		else
			fv = (fv - zlim [1]) / diff (zlim)
	}
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

	p0 = .mar3 (nhl)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.frame (axes [1] && ref.arrows [1], axes [2] && ref.arrows [2])
	title (main)
	.barsurf.labs (xlab, ylab)
	if (axes [1] && ! ref.arrows [1]) .barsurf.xlabs (xat, xlabs)
	if (axes [2] && ! ref.arrows [2]) .barsurf.ylabs (yat, ylabs)
	if (z.axis) .z.axis (zlab, zlim)

	line.width = .fine.line.width ()
	if (! grid)
		grid.color = NA
	colf = .ST (colf, colff, c (w1, w2), theme, NULL, "lum")
	colors1 = colf (w1)
	colors2 = colf (w2)
	for (i in (n - 1):1)
	{	for (j in (n - i):1)
		{	if (i < n - 1 && j < n - i)
			{	xsub = c (x [i], x [i + 1], x [i + 1])
				ysub = c (y [j + 1], y [j], y [j + 1])
				fsub = c (fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
				colorstr = colors1 [i, j]
				.barsurf.poly (xsub, ysub, fsub, colorstr, line.width, grid.color)
			}	
			xsub = c (x [i], x [i], x [i + 1])
			ysub = c (y [j], y [j + 1], y [j])
			fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j])
			colorstr = colors1 [i, j]
			.barsurf.poly (xsub, ysub, fsub, colorstr, line.width, grid.color)
		}
	}
	par (p0)
	.catchargs (...)
}

.is.const = function (flim, fv)
{	u = unique (as.vector (fv) )
	u = u [is.finite (u)]
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
