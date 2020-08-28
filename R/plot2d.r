#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.plot.new = function (main, xlab, ylab, xyrel, reverse, swap.sides, x, y)
{	if (xyrel == "f" || xyrel == "m") pty = "m"
	else if (xyrel == "s") pty = "s"
	else stop ("xyrel not in {f, s, m}")
	p0 = par (pty=pty, "mar")

	xlim = range (x)
	ylim = range (y)
	r = diff (ylim) / diff (xlim)
	if (reverse [1]) xlim = rev (xlim)
	if (reverse [2]) ylim = rev (ylim)

	if (xyrel == "f")
	{	p.dx = par ("pin")[1]
		p.dy = par ("pin")[2]
		p.dx.2 = p.dy /  r
		p.dy.2 = p.dx *  r
		if (p.dx == p.dx.2)
			NULL
		else
		{	plt = par ("plt")
			if (p.dx > p.dx.2)
			{	par (pin = c (p.dx.2, p.dy) )
				plt [1:2] = par ("plt")[1:2]
			}
			else
			{	par (pin = c (p.dx, p.dy.2) )
				plt [3:4] = par ("plt")[3:4]
			}
			par (plt=plt)
		}
	}

	plot.new ()
	plot.window (xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
	if (swap.sides [1]) mtext (ylab, 4, 3, cex = par ("cex") )
	else title (ylab=ylab)
	if (swap.sides [2])
	{	mtext (main, 1, 2, font=2, cex = par ("cex") * par ("cex.main") )
		mtext (xlab, 3, 3, cex = par ("cex") )
	}
	else title (main=main, xlab=xlab)
	p0
}

plot_dfield = function (x, y, fv, ..., fb,
	main="", xlab="x", ylab="y",
	contours=TRUE, grid=FALSE, heatmap=TRUE,
	contour.labels=FALSE, bin.labels=FALSE,
	add=FALSE, axes=TRUE, reverse=FALSE, swap.sides=FALSE,
	xyrel, continuous.axes=FALSE,
	ncontours=2, clabs, blabs,
	xat, yat, xlabs, ylabs,
	contour.color="#000000", grid.color="#888888", 
	colf, colff, colors, hcv=FALSE)
{	axes = .dbl (axes)
	reverse = .dbl (reverse)
	swap.sides = .dbl (swap.sides)
	continuous.axes = .dbl (continuous.axes)
	if (missing (xyrel) )
		xyrel = test.xyrel (x, y, fv)

	nx = ny = xb = yb = 0
	.UNPACK (.val.xy (x, y, fv, TRUE) )

	if (axes [1]) .UNPACK (.axis.val (x, xat, xlabs, continuous.axes [1], names = rownames (fv) ) )
	if (axes [2]) .UNPACK (.axis.val (y, yat, ylabs, continuous.axes [2], names = colnames (fv) ) )

	if (bin.labels)
	{	if (missing (blabs) )
			blabs = fv
		else
			.val.dfield.matrix (nx, ny, blabs)
	}

	if (missing (colors) )
		colors = .extract.private.args (...)$cols
	if (heatmap && ! is.null (colors) )
		.val.dfield.matrix (nx, ny, colors)

	if (! add)
		p0 = .plot.new (main, xlab, ylab, xyrel, reverse, swap.sides, xb, yb)

	if (heatmap)
	{	if (is.null (colors) )
		{	colf = .ST (colf, colff, fv, hcv)
			colors = colf (fv)
		}
		.plot.heatmap (nx, ny, xb, yb, colors)
	}

	line.width = .fine.line.width ()
	if (grid)
	{	abline (v=xb, lwd=line.width, col=grid.color)
		abline (h=yb, lwd=line.width, col=grid.color)
	}
	if (contours && ncontours > 0)
		.plot.contours (x, y, fv, ncontours, fb, contour.labels, clabs, contour.color)
	if (bin.labels)
		text (rep (x, times=ny), rep (y, each=nx), blabs)

	if (! add)
	{	.box (axes, reverse, xat, yat, xlabs, ylabs, swap.sides)
		par (p0)
	}
	.catchargs (...)
}

plot_matrix = function (x, y, fv, ..., transpose=TRUE,
	xlab, ylab,
	contours=FALSE, grid, bin.labels,
	reverse = c (FALSE, transpose), swap.sides=reverse,
	blabs, colors,
	hcv=TRUE)
{	v = .val.xy (x, y, fv, TRUE, transpose=transpose)

	if (missing (grid) )
		grid = (v$nx <= 20 && v$ny <= 20)
	if (missing (bin.labels) )
		bin.labels = (v$nx <= 10 && v$ny <= 10)

	if (transpose)
	{	if (missing (xlab) ) xlab = "col"
		if (missing (ylab) ) ylab = "row"
		if (bin.labels && ! missing (blabs) )
		{	.val.dfield.matrix (v$ny, v$nx, blabs)
			blabs = t (blabs)

		}
		if (! missing (colors) )
		{	.val.dfield.matrix (v$ny, v$nx, colors)
			colors = t (colors)
		}
	}
	else
	{	if (missing (xlab) ) xlab = "row"
		if (missing (ylab) ) ylab = "col"
	}

	plot_dfield (x, y, v$fv,
		xlab=xlab, ylab=ylab, 
		contours=contours, grid=grid, bin.labels=bin.labels,
		reverse=reverse, swap.sides=swap.sides,
		blabs=blabs, colors=colors,
		hcv=hcv, ...)
}

plot_cfield = function (x, y, fv, ..., fb,
	main="", xlab="x", ylab="y",
	contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
	add=FALSE, axes=TRUE, reverse=FALSE, swap.sides=FALSE,
	ncontours=6, clabs,
	xyrel, xat, yat, xlabs, ylabs,
	contour.color="#000000", colf, colff, hcv=FALSE)
{	axes = .dbl (axes)
	reverse = .dbl (reverse)
	swap.sides = .dbl (swap.sides)
	if (missing (xyrel) )
		xyrel = test.xyrel (x, y, fv)

	nx = ny = 0
	.UNPACK (.val.xy (x, y, fv) )

	if (! add)
		p0 = .plot.new (main, xlab, ylab, xyrel, reverse, swap.sides, x, y)
	if (heatmap)
	{	w = matrix (0, nrow=nx - 1, ncol=ny - 1)
		for (i in 1:(nx - 1) )
		{	for (j in 1:(ny - 1) )
			{	fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
				w [i, j] = mean (fsub)
			}
		}
		colf = .ST (colf, colff, w, hcv)
		colors = colf (w)
		.plot.heatmap (nx - 1, ny - 1, x, y, colors)
	}
	if (contours && ncontours > 0)
		.plot.contours (x, y, fv, ncontours, fb, contour.labels, clabs, contour.color)
	if (! add)
	{	.box (axes, reverse, xat, yat, xlabs, ylabs, swap.sides)
		par (p0)
	}
	.catchargs (...)
}

plot_tricontour = function (x, y, fv, ..., fb,
	main="", xlab="x", ylab="y",
	contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
	axes=TRUE, ncontours=6, clabs, xyrel="s",
	xat, yat, xlabs, ylabs,
	contour.color="#000000", colf, colff, hcv=FALSE)
{	n = 0
	.UNPACK (.val.tri (fv) )
	x = y = seq (0, 1, length.out=n)

	axes = .dbl (axes)
	
	p0 = .plot.new (main, xlab, ylab, xyrel, c (FALSE, FALSE), c (FALSE, FALSE), x, y)
	if (heatmap)
	{	w1 = w2 = matrix (NA, nrow=n, ncol=n)
		for (i in 1:(n - 1) )
		{	for (j in 1:(n - i) )
			{	fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j])
				w1 [i, j] = mean (fsub)
			}
		}
		for (i in 1:(n - 2) )
		{	if (i < n - 1)
			{	for (j in 1:(n - i - 1) )
				{	fsub = c (fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
					w2 [i, j] = mean (fsub)
				}
			}
		}

		colf = .ST (colf, colff, c (w1, w2), hcv)
		colors1 = colf (w1)
		colors2 = colf (w2)
		for (i in 1:(n - 1) )
		{	for (j in 1:(n - i) )
			{	xsub = c (x [i], x [i], x [i + 1])
				ysub = c (y [j], y [j + 1], y [j])
				col = colors1 [i, j]
				polygon (xsub, ysub, border=col, col=col)
			}
		}
		for (i in 1:(n - 2) )
		{	if (i < n - 1)
			{	for (j in 1:(n - i - 1) )
				{	xsub = c (x [i], x [i + 1], x [i + 1])
					ysub = c (y [j + 1], y [j], y [j + 1])
					col = colors2 [i, j]
					polygon (xsub, ysub, border=col, col=col)
				}
			}
		}
	}
	if (contours && ncontours > 0)
		.plot.contours (x, y, fv, ncontours, fb, contour.labels, clabs, contour.color)
	polygon (c (0, 1, 1), c (1, 0, 1), border=NA, col="white")
	polygon (c (0, 0, 1), c (0, 1, 0) )
	if (axes [1]) .axis (1, xat, xlabs)
	if (axes [2]) .axis (2, yat, ylabs)
	par (p0)
	.catchargs (...)
}

.box = function (axes, reverse, xat, yat, xlabs, ylabs, swap.sides)
{	box ()
	if (axes [1])
	{	if (swap.sides [2]) .axis (3, xat, xlabs)
		else .axis (1, xat, xlabs)
	}
	if (axes [2])
	{	if (swap.sides [1]) .axis (4, yat, ylabs)
		else .axis (2, yat, ylabs)
	}
}

.axis = function (s, at, labs)
{	if (missing (at) )
		axis (s)
	else if (missing (labs) )
		axis (s, at)
	else
		axis (s, at, labs)
}

.plot.contours = function (x, y, fv, ncontours, fb, contour.labels, clabs, contour.color)
{	if (length (contour.color) < ncontours)
		contour.color = rep (contour.color, ncontours)
	if (diff (range (fv, na.rm=TRUE) ) != 0)
	{	if (missing (fb) )
		{	N = ncontours + 2
			flim = range (fv, na.rm=TRUE)
			fb = seq (flim [1], flim [2], length.out=N)[-c (1, N)]
		}
		else
			ncontours = length (fb)
		if (contour.labels)
		{	if (missing (clabs) )
				clabs = signif (fb, 3)
			else if (ncontours != length (clabs) )
				stop ("ncontours != length (clabs)")
			contour (x, y, fv, levels=fb, labcex=0.8, drawlabels=TRUE, labels=clabs, add=TRUE, col=contour.color)
		}
		else
			contour (x, y, fv, levels=fb, drawlabels=FALSE, add=TRUE, col=contour.color)
	}
}

.plot.heatmap = function (nr, nc, x, y, colors)
{	for (i in 1:nr)
	{	for (j in 1:nc)
			rect (x [i], y [j], x [i + 1], y [j + 1], lwd=0.125, border = colors [i, j], col = colors [i, j])
	}
}

test.xyrel = function (x, y, fv)
{	f = FALSE
	if (missing (x) && missing (y) )
	{	r = (nrow (fv) / ncol (fv) )
		f = (r > 0.099 && r < 10.001) 
	}
	else if (! missing (x) && ! missing (y) )
	{	r = diff (range (x, na.rm=TRUE) ) / diff (range (y, na.rm=TRUE) )
		f = (r > 0.099 && r < 10.001)
	}
	if (f)
		"f"
	else
		"m"
}

.val.dfield.matrix = function (nx, ny, input.matrix)
{	mname = as.character (substitute (input.matrix) )
	if (is.matrix (input.matrix) )
	{	if (nx != nrow (input.matrix) || ny != ncol (input.matrix) )
			stop (sprintf ("fv and %s different dimensions", mname) )
	}
	else
		stop (sprintf ("%s not matrix"), mname)
}
