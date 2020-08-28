#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.valx = function (x, any.order.ok=FALSE, xname)
{	if (missing (xname) )
		xname = as.character (substitute (x) )

	x = as.numeric (x)
	if (length (unique (x) ) != length (x) )
		stop ("needs unique coordinate values")
	dx = diff (x)
	if (all (dx > 0) ) type = "+"
	else if (all (dx < 0) ) type = "-"
	else if (any.order.ok) type = "s"
	else stop ("needs sorted coordinate values")

	if (xname == "x") list (xtype=type, x=x)
	else if (xname == "y") list (ytype=type, y=x)
	else if (xname == "z") list (ztype=type, z=x)
	else stop ()
}

.val.xy = function (x, y, fv, discrete=FALSE, ..., reverse = c (FALSE, FALSE), sort=TRUE, transpose=FALSE, matrix.name="fv")
{	fv = .as.numeric.matrix (fv)
	if (transpose)
		fv = t (fv)

	nx = nrow (fv)
	ny = ncol (fv)
	if (nx < 2 || ny < 2)
		stop ("nrows or ncols < 2")

	xtype = ytype = "+"
	if (missing (x) ) x = 1:nx
	else .UNPACK (.valx (x) )
	if (missing (y) ) y = 1:ny
	else .UNPACK (.valx (y) )

	xb = yb = 0
	if (discrete)
	{	.UNPACK (.val.xy.ext (nx, x) )
		.UNPACK (.val.xy.ext (ny, y) )
	}
	else
	{	if (nx != length (x) ) stop ("nrows != length (x)")
		if (ny != length (y) ) stop ("ncols != length (y)")
	}

	xrev = ( (xtype == "-") + reverse [1] == 1)
	yrev = ( (ytype == "-") + reverse [2] == 1)
	if (xrev)
	{	fv = fv [nx:1,]
		x = rev (x)
		xb = rev (xb)
		
	}
	if (yrev)
	{	fv = fv [,ny:1]
		y = rev (y)
		yb = rev (yb)
	}

	s = c ("nx", "ny", matrix.name, "x", "y", "xb", "yb")
	v = list (nx, ny, fv, x, y, xb, yb)
	names (v) = s
	v
}

.val.xy.ext = function (nx, x)
{	xname = as.character (substitute (x) )

	if (nx == length (x) )
	{	xb = (x [-nx] + x [-1]) / 2
		xb = c (2 * x [1] - xb [1], xb, 2 * x [nx] - xb [nx - 1])
	}
	else if (nx == length (x) - 1)
	{	xb = x
		x = (x [-(nx + 1)] + x [-1]) / 2
	}
	else
		stop (sprintf ("nrows != length (%s) or length (%s) - 1", xname, xname) )
	if (xname == "x") list (x=x, xb=xb)
	else if (xname == "y") list (y=x, yb=xb)
}

.val.tri = function (fv)
{	nx = ny = 0
	.UNPACK (.val.xy (,,fv) )
	n = nx

	if (nx != ny)
		stop ("need square matrix")
	if (n < 3)
		stop ("nrows and ncols < 3")

	for (i in 2:n)
	{	for (j in n:(n - i + 2) )
			fv [i, j] = NA
	}

	list (n=n, fv=fv)
}

.val.cf3d = function (u, v, z, fv, reverse)
{	if (! is.list (fv) )
		stop ("fv not list")
	nz = length (fv)
	if (nz < 2)
		stop ("need 2 or more slides")

	nx = ny = x = y = temp = 0
	.UNPACK (.val.xy (u, v, fv [[1]], reverse=reverse, matrix.name="temp") )
	fv [[1]] = temp

	for (i in 2:nz)
	{	fv [[i]] = .val.xy (u, v, fv [[i]], reverse=reverse)$fv
		if (nx != nrow (fv [[i]]) || ny != ncol (fv [[i]]) )
			stop ("all fv matrices need to be same size")
	}
	if (missing (z) )
	{	ztype = "+"
		z = 1:nz
	}
	else
	{	.UNPACK (.valx (z, TRUE) )
		if (nz != length (z) )
			stop ("nslides != length (z) )")
	}

	list (nx=nx, ny=ny, nz=nz, fv=fv, ztype=ztype, x=x, y=y, z=z)
}

.val.vec2 = function (x, y, dx, dy)
{	nx = ny = fv = 0

	r1 = .val.xy (x, y, dx, matrix.name="dx")
	r2 = .val.xy (x, y, dy, matrix.name="dy")
	.UNPACK (r1)
	dy = r2$dy

	if (nx < 4 || ny < 4)
		stop ("nrows or ncols < 4")
	if (nx != nrow (dy) || ny != ncol (dy) )
		stop ("dx and dy have different dimensions")

	list (nx=nx, ny=ny, dx=dx, dy=dy, x=x, y=y)
}

.val.vec3 = function (x, y, z, dx, dy, dz)
{	dx = .as.numeric.array3 (dx)
	dy = .as.numeric.array3 (dy)
	dz = .as.numeric.array3 (dz)
	
	nx = dim (dx)[1]
	ny = dim (dx)[2]
	nz = dim (dx)[3]

	if (nx < 4 || ny < 4 || nz < 4)
		stop ("nrows, ncols or nsheets < 4")
	if (nx != dim (dy)[1] || ny != dim (dy)[2] || nz != dim (dy)[3] ||
		nx != dim (dz)[1] || ny != dim (dz)[2] || nz != dim (dz)[3])
		stop ("dx, dy and dz have different dimensions")

	xtype = ytype = ztype = "+"
	if (missing (x) ) x = 1:nx
	else .UNPACK (.valx (x) )
	if (missing (y) ) y = 1:ny
	else .UNPACK (.valx (y) )
	if (missing (z) ) z = 1:nz
	else .UNPACK (.valx (z) )

	if (nx != length (x) ) stop ("x-dim size != length (x)")
	if (ny != length (y) ) stop ("y-dim size != length (y)")
	if (nz != length (z) ) stop ("z-dim size != length (z)")

	xrev = (xtype == "-")
	yrev = (ytype == "-")
	zrev = (ztype == "-")
	if (xrev)
	{	x = rev (x)
		dx = dx [nx:1,,]
		dy = dy [nx:1,,]
		dz = dz [nx:1,,]
	}
	if (yrev)
	{	y = rev (y)
		dx = dx [,ny:1,]
		dy = dy [,ny:1,]
		dz = dz [,ny:1,]
	}
	if (zrev)
	{	z = rev (z)
		dx = dx [,,nz:1]
		dy = dy [,,nz:1]
		dz = dz [,,nz:1]
	}

	list (nx=nx, ny=ny, nz=nz, dx=dx, dy=dy, dz=dz, x=x, y=y, z=z)
}


.axis.val = function (x, xat, xlabs, continuous=TRUE, xname, names=NULL)
{	if (missing (xname) )
		xname = as.character (substitute (x) )

	if (missing (xat) )
	{	if (continuous)
		{	if (! missing (xlabs) )
				warning (sprintf ("%sat neeeded, %slabs ignored", xname, xname) )
			xat = pretty (x, 6)
			n = length (xat)
			xat = xat [2:(n - 1)]
			xlabs = xat
		}
		else
		{	if (missing (xlabs) )
			{	if (is.null (names) )
				{	n = length (x)
					if (n > 20)
					{	I = pretty (c (1, n), 6)
						n = length (I)
						I = I [2:(n - 1)]
						I = as.integer (I)
						xat = x [I]
						xlabs = xat
					}
					else
						xat = xlabs = x
				}
				else
				{	xat = x
					xlabs = names
				}
			}
			else
			{	xat = x
				xlabs = as.character (xlabs)
				if (length (xat) != length (xlabs) )
					stop (sprintf ("length (%sat) != length (%slabs)", xname, xname) )
			}
		}
	}
	else
	{	xat = as.numeric (xat)
		if (missing (xlabs) )
			xlabs = xat
		else
		{	xlabs = as.character (xlabs)
			if (length (xat) != length (xlabs) )
				stop (sprintf ("length (%sat) != length (%slabs)", xname, xname) )
		}
		
	}

	if (xname == "x") list (xat=xat, xlabs=xlabs)
	else if (xname == "y") list (yat=xat, ylabs=xlabs)
	else stop ()
}

.axis.scale = function (x, xat, reverse=FALSE, nx = length (x), xname, xlim)
{	if (missing (xname) )
		xname = as.character (substitute (x) )

	if (missing (xlim) )
	{	xlim = range (x)
		if (reverse)
			xlim = rev (xlim)
	}

	x = (x - xlim [1]) / diff (xlim)
	if (missing (xat) ) xat = 0
	else xat = (xat - xlim [1]) / diff (xlim)

	if (xname == "x" || xname == "xb") s = c (xname, "xat")
	else if (xname == "y" || xname == "yb") s = c (xname, "yat")
	else if (xname == "z") s = c (xname, "z0")
	else stop ()

	v = list (x, xat)
	names (v) = s
	v
}
