plot3d.bar = function (xb, yb, z,
	main, xlab="x", ylab="y", xlabs, ylabs,
	colv.1, colv.2, colvs.sms, reverse=FALSE,
	zlim, ...)
{	is.nested = FALSE
	if (inherits (z, "P.nmatrix") )
	{	nsm = length (z$sms)
		if (nsm > 0)
		{	is.nested = TRUE
			sms = z$sms

			if (missing (colvs.sms) )
			{	v1 = c (75, 25, 67.5)
				v2 = c (255, 25, 67.5)
				if (nsm == 1)
					colvs.sms = rbind (v1)
				else
				{	w = seq (0, 1, length.out=nsm)
					colvs.sms = matrix (0, nsm, 3)
					for (i in 1:nsm)
						colvs.sms [i,] = v1 + (v2 - v1) * w [i]
				}
			}
			else
			{	colvs.sms = rbind (colvs.sms)
				ncolvs = nrow (colvs.sms)
				if (nsm > 1 && ncolvs == 1)
					colvs.sms = matrix (rep (colvs.sms, each=nsm), nsm)
				else if (nsm != nrow (colvs.sms) )
					stop ("nrow (colvs.sms) is unsuitable")
			}

			if (reverse)
			{	sms = rev (sms)
				colvs.sms = colvs.sms [nsm:1,]
			}
		}
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
	xb = (xb - min (xb) ) / diff (range (xb) )
	yb = (yb - min (yb) ) / diff (range (yb) )
	if (missing (zlim) )
		zlim = range (z, na.rm=TRUE)
    dz = diff (zlim)
    if (dz == 0)
		z [] = 0.5
	else
	    z = (z - zlim [1]) / dz

	if (missing (main) )
		main = ""
	with.xlabs = (!missing (xlabs) )
	with.ylabs = (!missing (ylabs) )

	if (missing (colv.1) )
		colv.1 = getOption ("barsurf")$plot3d.bar.colv.1
	if (missing (colv.2) )
		colv.2 = getOption ("barsurf")$plot3d.bar.colv.2

	colstr.1 = hcl (colv.1 [1], colv.1 [2], colv.1 [3])
	colstr.2 = hcl (colv.2 [1], colv.2 [2], colv.2 [3])

	if (is.nested)
	{	colstrs.sms = hcl (colvs.sms [,1], colvs.sms [,2], colvs.sms [,3])
		colstr.1 = matrix (colstr.1, nrow=nx, ncol=ny)
		for (i in 1:nsm)
		{	sm = sms [[i]]
			colstr.1 [sm$x1:sm$x2, sm$y1:sm$y2] = colstrs.sms [i]
		}
	}

	p0 = par (mar=c (1, 0.2, 1, 0.2), ...)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.plane.xy (!with.xlabs, !with.ylabs)
	.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	title (main)
	.barsurf.labs (xlab, ylab)

	if (with.xlabs)
	{	xm = (xb [-(nx + 1)] + xb [-1]) / 2
		xlabs = substring (xlabs, 1, 3)
		.barsurf.xlabs (nx, xm, xlabs)
	}
	if (with.ylabs)
	{	ym = (yb [-(ny + 1)] + yb [-1]) / 2
		ylabs = substring (ylabs, 1, 3)
		.barsurf.ylabs (ny, ym, ylabs)
	}

	for (i in nx:1)
	    for (j in ny:1)
    	{	if (is.na (z [i, j]) )
				NULL
			else if (is.nested)
				.barsurf.bar (xb [i], xb [i + 1], yb [j], yb [j + 1], z [i, j], colstr.1 [i, j], colstr.2)
			else
				.barsurf.bar (xb [i], xb [i + 1], yb [j], yb [j + 1], z [i, j], colstr.1, colstr.2)
		}

    par (p0)
}

plot3d.surface = function (x, y, z,
	main, xlab="x", ylab="y",
	colv.1, colv.2,
	zlim, ...)
{	nx = nrow (z)
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
	x = (x - min (x) ) / diff (range (x) )
	y = (y - min (y) ) / diff (range (y) )
	if (missing (zlim) )
		zlim = range (z)
    dz = diff (zlim)
    if (dz == 0)
		z [] = 0.5
	else
	    z = (z - zlim [1]) / dz

	w = matrix (0, nrow=nx - 1, ncol=ny - 1)
	for (i in 1:(nx - 1) )
	    for (j in 1:(ny - 1) )
	{	zsub = c (z [i, j], z [i, j + 1], z [i + 1, j], z [i + 1, j + 1])
		w [i, j] = max (zsub) - min (zsub)
	}
	wlim = range (w)
	dw = diff (wlim)
	if (dw < 1e-6)
		w [] = 0.5
	else
		w = (w - wlim [1]) / dw

	if (missing (main) )
		main = ""

	if (missing (colv.1) )
		colv.1 = getOption ("barsurf")$plot3d.surface.colv.1
	if (missing (colv.2) )
		colv.2 = getOption ("barsurf")$plot3d.surface.colv.2

	colv.1 = hcl2rgb (colv.1)
	colv.2 = hcl2rgb (colv.2)

	p0 = par (mar=c (1, 0.2, 1, 0.2), ...)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.plane.xy ()
	.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	title (main)
	.barsurf.labs (xlab, ylab)

	for (i in (nx - 1):1)
	    for (j in (ny - 1):1)
    	{	xsub = c (x [i], x [i], x [i + 1], x [i + 1])
			ysub = c (y [j], y [j + 1], y [j + 1], y [j])
			zsub = c (z [i, j], z [i, j + 1], z [i + 1, j + 1], z [i + 1, j])
			colstr = .interpolate.rgb (colv.1, colv.2, w [i, j])
    		.barsurf.poly (xsub, ysub, zsub, colstr)
	    }

	par (p0)
}

plot3d.surf = function (...)
	plot3d.surface (...)

plot3d.trisurface = function (x, y, z,
	main, xlab="x", ylab="y",
	colv.1, colv.2,
	zlim, ...)
{	n = nrow (z)
	if (n != ncol (z) )
		stop ("z needs to be square matrix")
	if (n < 3)
		stop ("nrow (z) < 3")
	x = 1:n
	y = 1:n
	z = lr2na (z)
	x = (x - min (x) ) / diff (range (x) )
	y = (y - min (y) ) / diff (range (y) )
	if (missing (zlim) )
		zlim = range (z, na.rm=TRUE)
    dz = diff (zlim)
    if (dz == 0)
		z [] = 0.5
	else
	    z = (z - zlim [1]) / dz

	w1 = w2 = matrix (NA, nrow=n, ncol=n)
	for (i in 1:(n - 1) )
	{	for (j in 1:(n - i) )
		{	zsub = c (z [i, j], z [i, j + 1], z [i + 1, j])
			w1 [i, j] = max (zsub) - min (zsub)
		}
	}
	for (i in 1:(n - 2) )
	{	if (i < n - 1)
		{	for (j in 1:(n - i - 1) )
			{	zsub = c (z [i, j + 1], z [i + 1, j], z [i + 1, j + 1])
				w2 [i, j] = max (zsub) - min (zsub)
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

	if (missing (main) )
		main = ""

	if (missing (colv.1) )
		colv.1 = getOption ("barsurf")$plot3d.surface.colv.1
	if (missing (colv.2) )
		colv.2 = getOption ("barsurf")$plot3d.surface.colv.2

	colv.1 = hcl2rgb (colv.1)
	colv.2 = hcl2rgb (colv.2)

	p0 = par (mar=c (1, 0.2, 1, 0.2), ...)
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.barsurf.plane.xy ()
	.barsurf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.barsurf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	title (main)
	.barsurf.labs (xlab, ylab)

	for (i in 1:(n - 1) )
	{	for (j in 1:(n - i) )
		{	xsub = c (x [i], x [i], x [i + 1])
			ysub = c (y [j], y [j + 1], y [j])
			zsub = c (z [i, j], z [i, j + 1], z [i + 1, j])
			colstr = .interpolate.rgb (colv.1, colv.2, w1 [i, j])
			.barsurf.poly (xsub, ysub, zsub, colstr)
		}
	}
	for (i in 1:(n - 2) )
	{	if (i < n - 1)
		{	for (j in 1:(n - i - 1) )
			{	xsub = c (x [i], x [i + 1], x [i + 1])
				ysub = c (y [j + 1], y [j], y [j + 1])
				zsub = c (z [i, j + 1], z [i + 1, j], z [i + 1, j + 1])
				colstr = .interpolate.rgb (colv.1, colv.2, w2 [i, j])
				.barsurf.poly (xsub, ysub, zsub, colstr)
			}
		}
	}

	par (p0)
}
