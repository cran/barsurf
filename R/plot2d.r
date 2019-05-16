plot2d.cell = function (xb, yb, z,
	grid.lines=TRUE,
	main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
	colv.1, colv.2, colv.na, contrast=0,
	pty="s", ...)
{	is.nested = FALSE
	if (inherits (z, "P.nmatrix") )
	{	is.nested = TRUE
		sms = z$sms
		z = z$x
	}

	nx = nrow (z)
	ny = ncol (z)
	if (missing (xb) )
		xb = 0:nx
	else if (length (xb) != nx + 1)
		stop ("length (xb) != nrow (z)")
	if (missing (yb) )
		yb = 0:ny
	else if (length (yb) != ny + 1)
		stop ("length (yb) != ncol (z)")
	z = .transform (contrast, z)
	zlim = range (z, na.rm=TRUE)
	dz = diff (zlim)
	if (dz == 0)
		z [] = 0.5
	else
		z = (z - zlim [1]) / dz

	if (missing (xat) )
	{	xat = (xb [-(nx + 1)] + xb [-1]) / 2
		if (missing (xlabs) )
		{	rnames = rownames (z)
			if (is.null (rnames) )
				xlabs = 1:nx
			else
				xlabs = rnames
		}
	}
	else if (missing (xlabs) )
		xlabs = format (signif (xat, 2) )
	if (missing (yat) )
	{	yat = (yb [-(ny + 1)] + yb [-1]) / 2
		if (missing (ylabs) )
		{	cnames = colnames (z)
			if (is.null (cnames) )
				ylabs = 1:ny
			else
				ylabs = cnames
		}	
	}
	else if (missing (ylabs) )
		ylabs = format (signif (yat, 2) )
	if (missing (main) )
		main = ""

	if (missing (colv.1) )
		colv.1 = getOption ("barsurf")$plot2d.cell.colv.1
	if (missing (colv.2) )
		colv.2 = getOption ("barsurf")$plot2d.cell.colv.2
	if (missing (colv.na) )
		colv.na = getOption ("barsurf")$plot2d.cell.colv.na

	p0 = par (pty=pty, ...)
	plot.new ()
	plot.window (xlim=range (xb), ylim=range (yb), xaxs="i", yaxs="i")
	axis (1, xat, xlabs)
	axis (2, yat, ylabs)
	title (main=main, xlab=xlab, ylab=ylab)

	for (i in 1:nx)
		for (j in 1:ny)
		{	if (is.na (z [i, j]) )
				colstr = hcl (colv.na [1], colv.na [2], colv.na [3])
			else
				colstr = .interpolate.hcl (colv.1, colv.2, z [i, j])
			rect (xb [i], yb [j], xb [i + 1], yb [j + 1], border=colstr, col=colstr)
		}

	if (is.nested)
	{	for (sm in sms)
			rect (xb [sm$x1], yb [sm$y1], xb [sm$x2 + 1], yb [sm$y2 + 1])
	
	}
	else if (grid.lines)
		abline (v=xb [2:nx], h=yb [2:ny])

	box ()
	par (p0)
}

plot2d.contour = function (x, y, z, zb,
	contours=TRUE, heat.map=TRUE,
	main, xlab="x", ylab="y",
	colv.1, colv.2, contrast=0,
	pty="s", ...)
{	N = 8

	nx = nrow (z)
	ny = ncol (z)
	if (nx < 2 || ny < 2)
		stop ("nrow(z) or ncol(z) < 2")
	if (missing (x) )
		x = 1:nx
	else if (length (x) != nx)
		stop ("length (x) != nrow (z)")
	if (missing (y) )
		y = 1:ny
	else if (length (y) != ny)
		stop ("length (y) != ncol (z)")
	zlim = range (z, na.rm=TRUE)

	if (missing (main) )
		main = ""

	p0 = par (pty=pty, ...)
	plot.new ()
	plot.window (xlim=range (x), ylim=range (y), xaxs="i", yaxs="i")
	title (main=main, xlab=xlab, ylab=ylab)
	axis (1)
	axis (2)

	if (heat.map)
	{	if (missing (colv.1) )
			colv.1 = getOption ("barsurf")$plot2d.contour.colv.1
		if (missing (colv.2) )
			colv.2 = getOption ("barsurf")$plot2d.contour.colv.2

		w = matrix (0, nrow=nx - 1, ncol=ny - 1)
		z2 = .transform (contrast, z)
		for (i in 1:(nx - 1) )
			for (j in 1:(ny - 1) )
		{	zsub = c (z2 [i, j], z2 [i, j + 1], z2 [i + 1, j], z2 [i + 1, j + 1])
			w [i, j] = mean (zsub)
		}
		wlim = range (w)
		dw = diff (wlim)
		if (dw == 0)
			w [] = 0.5
		else
			w = (w - wlim [1]) / dw
	
		for (i in 1:(nx - 1) )
			for (j in 1:(ny - 1) )
			{	xsub = c (x [i], x [i + 1])
				ysub = c (y [j], y [j + 1])
				colstr = .interpolate.hcl (colv.1, colv.2, w [i, j])
				rect (x [i], y [j], x [i + 1], y [j + 1], border=colstr, col=colstr)
			}
	}
	if (contours)
	{	if (missing (zb) )
			zb = seq (zlim [1], zlim [2], length.out=N)[-c (1, N)]
		contour (x, y, z, levels=zb, drawlabels=FALSE, add=TRUE)
	}

	box ()
	par (p0)
}

plot2d.tricontour = function (x, y, z, zb,
	contours=TRUE, heat.map=TRUE,
	main, xlab="x", ylab="y",
	colv.1, colv.2, contrast=0,
	pty="s", ...)
{	N = 8

	n = .test.z (z)
	if (missing (x) )
		x = 1:n
	else if (length (x) != n)
		stop ("length (x) != nrow (z)")
	if (missing (y) )
		y = 1:n
	else if (length (y) != n)
		stop ("length (y) != ncol (z)")
	z = lr2na(z)
	zlim = range (z, na.rm=TRUE)

	if (heat.map)
	{	w1 = w2 = matrix (NA, nrow=n, ncol=n)
		z2 = .transform (contrast, z)
		for (i in 1:(n - 1) )
		{	for (j in 1:(n - i) )
			{	zsub = c (z2 [i, j], z2 [i, j + 1], z2 [i + 1, j])
				w1 [i, j] = mean (zsub)
			}
		}
		for (i in 1:(n - 2) )
		{	if (i < n - 1)
			{	for (j in 1:(n - i - 1) )
				{	zsub = c (z2 [i, j + 1], z2 [i + 1, j], z2 [i + 1, j + 1])
					w2 [i, j] = mean (zsub)
				}
			}
		}
		wlim.1 = range (w1, na.rm=TRUE)
		wlim.2 = range (w2, na.rm=TRUE)
		wlim = range (c (wlim.1, wlim.2) )
		dw = diff (wlim)
		if (dw == 0)
			w1 [] = w2 [] = 0.5
		else
		{	w1 = (w1 - wlim [1]) / dw
			w2 = (w2 - wlim [1]) / dw
		}
	}

	if (missing (main) )
		main = ""

	if (missing (colv.1) )
		colv.1 = getOption ("barsurf")$plot2d.contour.colv.1
	if (missing (colv.2) )
		colv.2 = getOption ("barsurf")$plot2d.contour.colv.2

	p0 = par (pty=pty, ...)
	plot.new ()
	plot.window (xlim=range (x), ylim=range (y), xaxs="i", yaxs="i")
	title (main=main, xlab=xlab, ylab=ylab)

	if (heat.map)
	{	for (i in 1:(n - 1) )
		{	for (j in 1:(n - i) )
			{	xsub = c (x [i], x [i], x [i + 1])
				ysub = c (y [j], y [j + 1], y [j])
				zsub = c (z [i, j], z [i, j + 1], z [i + 1, j])
				colstr = .interpolate.hcl (colv.1, colv.2, w1 [i, j])
				polygon (xsub, ysub, border=colstr, col=colstr)
			}
		}
		for (i in 1:(n - 2) )
		{	if (i < n - 1)
			{	for (j in 1:(n - i - 1) )
				{	xsub = c (x [i], x [i + 1], x [i + 1])
					ysub = c (y [j + 1], y [j], y [j + 1])
					zsub = c (z [i, j + 1], z [i + 1, j], z [i + 1, j + 1])
					colstr = .interpolate.hcl (colv.1, colv.2, w2 [i, j])
					polygon (xsub, ysub, border=colstr, col=colstr)
				}
			}
		}
	}
	if (contours)
	{	if (missing (zb) )
			zb = seq (zlim [1], zlim [2], length.out=N)[-c (1, N)]
		contour (x, y, z, levels=zb, drawlabels=FALSE, add=TRUE)
	}

	x1 = x [1]
	x2 = x [n]
	y1 = y [1]
	y2 = y [n]
	polygon (c (x1, x2, x2), c (y2, y1, y2), border=NA, col="white")
	polygon (c (x1, x1, x2), c (y1, y2, y1) )
	axis (1, lwd=0, lwd.ticks=par ("lwd") )
	axis (2, lwd=0, lwd.ticks=par ("lwd") )
	par (p0)
}

.test.z = function (z)
{	n = nrow (z)
	if (n != ncol (z) )
		stop ("z needs to be square matrix")
	if (n < 3)
		stop ("nrow (z) < 3")
	n
}

lr2na = function (z)
{	n = .test.z (z)
	for (i in 2:n)
		for (j in n:(n - i + 2) )
			z [i, j] = NA
	z
}
