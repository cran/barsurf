#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.axis.val = function (label, nx, rnames, xat, xlabs, x, with.labs=TRUE)
{	if (missing (x) )
	{	uat = 1:nx
		xb = c (uat - 0.5, nx + 0.5)
	}
	else if (length (x) == nx)
	{	uat = x
		xb = (x [-nx] + x [-1]) / 2
		xb = c (2 * uat [1] - xb [1], xb, 2 * uat [nx] - xb [nx - 1])
	}
	else if (length (x) == nx + 1)
	{	uat = (x [-(nx + 1)] + x [-1]) / 2
		xb = x
	}
	else
		stop (sprintf ("length (%s) unsuitable", label) )
	if (with.labs)
	{	if (missing (xat) )
		{	if (missing (xlabs) )
			{	if (is.null (rnames) )
					xlabs = format (signif (uat, 2) )
				else
					xlabs = rnames
			}
			else if (length (xlabs) != nx)
				stop (sprintf ("length (%slabs) unsuitable", label) )
		}
		else
		{	if (missing (xlabs) )
				xlabs = format (signif (xat, 2) )
			else if (length (xat) != length (xlabs) )
				stop (sprintf ("length (%sat) != length (%slabs)", label, label) )
		}
		list (uat, xlabs, xb)
	}
	else
		list (NA, NA, xb)
}

.plot.new = function (main, xlab, ylab, xyrel, reverse, x, y)
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
	if (missing (main) )
		main = ""
	if (reverse [1]) mtext (ylab, 4, 3, cex = par ("cex") )
	else title (ylab=ylab)
	if (reverse [2])
	{	mtext (main, 1, 2, font=2, cex = par ("cex") * par ("cex.main") )
		mtext (xlab, 3, 3, cex = par ("cex") )
	}
	else title (main=main, xlab=xlab)
	p0
}

plot_dfield = function (x, y, fv, fb, ...,
	grid.lines, contours=TRUE, heatmap=TRUE, bin.labels=FALSE, contour.labels=FALSE,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	xyrel = test.xyrel (x, y, fv), transpose=FALSE,
	add=FALSE, axes=TRUE, reverse=FALSE,
	ncontours=2, clabs, blabs,
	grid.color, contour.color="#000000",
	color.function, color.fit, colors, hcv=FALSE)
{	if (transpose)
	{	fv = t (fv)
		if (! missing (blabs) )
			blabs = t (blabs)
		if (! missing (colors) )
			colors = t (colors)
	}
	nx = nrow (fv)
	ny = ncol (fv)	
	xaxis = .axis.val ("x", nx, rownames (fv), xat, xlabs, x)
	yaxis = .axis.val ("y", ny, colnames (fv), yat, ylabs, y)
	if (missing (xat) )
		xat = xaxis [[1]]
	if (missing (yat) )
		yat = yaxis [[1]]
	xlabs = xaxis [[2]]
	xb = xaxis [[3]]
	ylabs = yaxis [[2]]
	yb = yaxis [[3]]
	if (missing (grid.lines) )
		grid.lines = (transpose && nx <= 20 && ny <= 20)

	axes = .dbl (axes)
	reverse = .dbl (reverse)
	grid.lines = .dbl (grid.lines)

	if (! add)
		p0 = .plot.new (main, xlab, ylab, xyrel, reverse, xb, yb)
	
	if (heatmap)
	{	if (missing (colors) )
		{	colors = .extract.cols (...)
			if (is.null (colors) )
			{	if (missing (color.function) )
				{	if (missing (color.fit) )
					{	if (hcv) color.fit = getOption ("barsurf")$litmus.fit.hcv
						else color.fit = getOption ("barsurf")$litmus.fit
						color.fit = eval (str2lang (color.fit) )
					}
					color.function = color.fit (fv)
				}
				colors = color.function (fv)
			}
		}
		.plot.heatmap (nx, ny, xb, yb, colors)
	}
	if (any (grid.lines) )
	{	v = .line.attr (TRUE, grid.color)
		if (grid.lines [1])
			abline (v=xb, lwd = v [[1]], col = v [[2]])
		if (grid.lines [2])
			abline (h=yb, lwd = v [[1]], col = v [[2]])
	}
	if (contours && ncontours > 0)
		.plot.contours (xaxis [[1]], yaxis [[1]], fv, ncontours, fb, contour.labels, clabs, contour.color)
	if (bin.labels)
	{	if (missing (blabs) )
			blabs = fv
		text (rep (xat, times=ny), rep (yat, each=nx), blabs)
	}
	if (! add)
	{	.box (axes, reverse, xat, yat, xlabs, ylabs)
		par (p0)
	}
}

plot_matrix = function (x, y, fv, fb, ...,
	grid.lines, contours=FALSE, heatmap=TRUE, bin.labels=FALSE, contour.labels=FALSE,
	main, xlab="col", ylab="row", xat, yat, xlabs, ylabs,
	xyrel = test.xyrel (x, y, fv), transpose=TRUE,
	add=FALSE, axes=TRUE, reverse = c (FALSE, transpose),
	ncontours=2, clabs, blabs,
	grid.color, contour.color="#000000",
	color.function, color.fit, colors, hcv=TRUE)
{	plot_dfield (x, y, fv, fb,
		grid.lines=grid.lines, contours=contours, heatmap=heatmap, bin.labels=bin.labels, contour.labels=contour.labels,
		main=main, xlab=xlab, ylab=ylab, xat=xat, yat=yat, xlabs=xlabs, ylabs=ylabs,
		xyrel=xyrel, transpose=transpose,
		add=add, axes=axes, reverse=reverse,
		ncontours=ncontours, clabs=clabs, blabs=blabs,
		grid.color=grid.color, contour.color=contour.color,
		color.function=color.function, color.fit=color.fit, colors=colors, hcv=hcv, ...)
}

plot_cfield = function (x, y, fv, fb, ...,
	contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	xyrel = test.xyrel (x, y, fv),
	add=FALSE, axes=TRUE, reverse=FALSE,
	ncontours=6, clabs,
	contour.color="#000000", color.function, color.fit, hcv=FALSE)
{	nx = nrow (fv)
	ny = ncol (fv)
	if (nx < 2 || ny < 2)
		stop ("nrow (fv) or ncol (fv) < 2")
	force (xyrel)
	if (missing (x) )
		x = 1:nx
	else if (length (x) != nx)
		stop ("length (x) != nrow (fv)")
	if (missing (y) )
		y = 1:ny
	else if (length (y) != ny)
		stop ("length (y) != ncol (fv)")

	axes = .dbl (axes)
	reverse = .dbl (reverse)

	if (! add)
		p0 = .plot.new (main, xlab, ylab, xyrel, reverse, x, y)

	if (heatmap)
	{	w = matrix (0, nrow=nx - 1, ncol=ny - 1)
		for (i in 1:(nx - 1) )
		{	for (j in 1:(ny - 1) )
			{	fsub = c (fv [i, j], fv [i, j + 1], fv [i + 1, j], fv [i + 1, j + 1])
				w [i, j] = mean (fsub)
			}
		}
		if (missing (color.function) )
		{	if (missing (color.fit) )
			{	if (hcv) color.fit = getOption ("barsurf")$litmus.fit.hcv
				else color.fit = getOption ("barsurf")$litmus.fit
				color.fit = eval (str2lang (color.fit) )
			}
			color.function = color.fit (w)
		}
		colors = color.function (w)
		.plot.heatmap (nx - 1, ny - 1, x, y, colors)
	}
	if (contours && ncontours > 0)
		.plot.contours (x, y, fv, ncontours, fb, contour.labels, clabs, contour.color)

	if (! add)
	{	.box (axes, reverse, xat, yat, xlabs, ylabs)
		par (p0)
	}
}

plot_tricontour = function (x, y, fv, fb, ...,
	contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
	main, xlab="x", ylab="y",
	xyrel="s",
	axes=TRUE,
	ncontours=6, clabs,
	contour.color="#000000", color.function, color.fit, hcv=FALSE)
{	n = .test.fv (fv)
	x = y = seq (0, 1, length.out=n)
	for (i in 2:n)
	{	for (j in n:(n - i + 2) )
			fv [i, j] = NA
	}

	axes = .dbl (axes)
	p0 = .plot.new (main, xlab, ylab, xyrel, c (FALSE, FALSE), x, y)

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
		if (missing (color.function) )
		{	if (missing (color.fit) )
			{	if (hcv) color.fit = getOption ("barsurf")$litmus.fit.hcv
				else color.fit = getOption ("barsurf")$litmus.fit
				color.fit = eval (str2lang (color.fit) )
			}
			color.function = color.fit (c (w1, w2) )
		}
		colors1 = color.function (w1)
		colors2 = color.function (w2)
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
	if (axes [1])
		axis (1)
	if (axes [2])
		axis (2)
	par (p0)
}

plot_vecfield = function (x, y, fx, fy, ...,
	vectors=TRUE, heatmap=TRUE, all=FALSE,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	xyrel = test.xyrel (x, y, fv),
	add=FALSE, axes=TRUE, reverse=FALSE,
	arrowh.length=1.75, arrowh.width = 0.75 * arrowh.length, arrow.color="#000000", fill.color="#08080810",
	color.function, color.fit, hcv=FALSE)
{	nx = nrow (fx)
	ny = ncol (fx)
	force (xyrel)
	if (nx < 2 || ny < 2)
		stop ("nrow (f*) or ncol (f*) < 2")
	if (nx != nrow (fy) || ny != ncol (fy) )
		stop ("fx and fy need same dimensions")
	if (missing (x) )
		x = 1:nx
	else if (length (x) != nx)
		stop ("length (x) != nrow (f*)")
	if (missing (y) )
		y = 1:ny
	else if (length (y) != ny)
		stop ("length (y) != ncol (f*)")

	axes = .dbl (axes)
	reverse = .dbl (reverse)

	if (! add)
		p0 = .plot.new (main, xlab, ylab, xyrel, reverse, x, y)

	if (heatmap)
	{	gv = sqrt (fx ^ 2 + fy ^ 2)

		w = matrix (0, nrow=nx - 1, ncol=ny - 1)
		for (i in 1:(nx - 1) )
		{	for (j in 1:(ny - 1) )
			{	gsub = c (gv [i, j], gv [i, j + 1], gv [i + 1, j], gv [i + 1, j + 1])
				w [i, j] = mean (gsub)
			}
		}
		if (missing (color.function) )
		{	if (missing (color.fit) )
			{	if (hcv) color.fit = getOption ("barsurf")$litmus.fit.hcv
				else color.fit = getOption ("barsurf")$litmus.fit.flow
				color.fit = eval (str2lang (color.fit) )
			}
			color.function = color.fit (w)
		}
		colors = color.function (w)
		.plot.heatmap (nx - 1, ny - 1, x, y, colors)
	}
	if (vectors && nx > 2 && ny > 2)
	{	rs = getOption ("barsurf")$rendering.style
		if (rs == "e")
			line.width = 0.125
		else
			line.width = 1

		dx = quantile (diff (x), 0.75)
		dy = quantile (diff (y), 0.75)
		hfx = max (fx, na.rm=TRUE)
		hfy = max (fy, na.rm=TRUE)
		if (all)
		{	a = 1
			b = 0
		}
		else
		{	a = 2
			b = 1
		}
		fu = fx [a:(nx - b), a:(ny - b)]
		fv = fy [a:(nx - b), a:(ny - b)]
		sfx = dx / hfx * fu / 2.1
		sfy = dy / hfy * fv / 2.1
		u = x [a:(nx - b)]
		v = y [a:(ny - b)]
		px = outer (u, rep (1, ny - 2 * b) )
		py = outer (rep (1, nx - 2 * b), v)
		x1 = px - sfx
		x2 = px + sfx
		y1 = py - sfy
		y2 = py + sfy
		.arrows (x1, y1, x2, y2, line.width, arrowh.length, arrowh.width, arrow.color, fill.color)
	}

	if (! add)
	{	.box (axes, reverse, xat, yat, xlabs, ylabs)
		par (p0)
	}
}

.box = function (axes, reverse, xat, yat, xlabs, ylabs)
{	box ()
	if (axes [1])
	{	if (reverse [2]) .axis (3, xat, xlabs)
		else .axis (1, xat, xlabs)
	}
	if (axes [2])
	{	if (reverse [1]) .axis (4, yat, ylabs)
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
		if (contour.labels)
		{	if (missing (clabs) )
				contour (x, y, fv, levels=fb, drawlabels=TRUE, add=TRUE, col=contour.color)
			else
			{	if (ncontours == length (clabs) )
					contour (x, y, fv, levels=fb, drawlabels=TRUE, labels=clabs, add=TRUE, col=contour.color)
				else
					stop ("ncontours != length (clabs)")
			}
		}
		else
			contour (x, y, fv, levels=fb, drawlabels=FALSE, add=TRUE, col=contour.color)
	}
}

.arrows = function (x0, y0, x1, y1, lwd=1, length=1.75, width=1.25, col="#000000", fill=col)
{	k = 0.0394
	min = abs (xinch (0.25 * k) )
	xd = abs (xinch (length * k) )
	yd = abs (yinch (width * k) )
	ip = rbind (-xd, 0)
	for (i in seq_len (length (x0) ) )
	{	pxd = x1 [i] - x0 [i]
		pyd = y1 [i] - y0 [i]
		h = sqrt (pxd^2 + pyd^2)
		if (! is.na (h) )
		{	if (1.25 * xd <= h)
			{	aps = rbind (c (0, -xd, -xd), c (0, yd, -yd) / 2)
				theta = atan2 (pyd, pxd)
				trans = matrix (c (cos (theta), sin (theta), -sin (theta), cos (theta) ), 2, 2)
				ip2 = trans %*% ip
				aps2 = trans %*% aps
				lines (c (x0 [i], x1 [i] + ip2 [1]), c (y0 [i], y1 [i] + ip2 [2]), lwd=lwd, col=col)
				polygon (aps2 [1,] + x1 [i], aps2 [2,] + y1 [i], lwd=lwd, border=col, col=fill)
			}
			else if (h > min)
				lines (c (x0 [i], x1 [i]), c (y0 [i], y1 [i]), lwd=lwd, col=col)
		}
	}
}

.plot.heatmap = function (nr, nc, x, y, colors)
{	for (i in 1:nr)
	{	for (j in 1:nc)
			rect (x [i], y [j], x [i + 1], y [j + 1], lwd=0.125, border = colors [i, j], col = colors [i, j])
	}
}

.test.fv = function (fv)
{	n = nrow (fv)
	if (n != ncol (fv) )
		stop ("fv needs to be square matrix")
	if (n < 3)
		stop ("nrow (fv) < 3")
	n
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
