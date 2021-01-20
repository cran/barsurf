#barsurf: Contour Plots, 3D Plots, Vector Fields and Heatmaps
#Copyright (C), Abby Spurdle, 2018 to 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.iso.x = function (which, x, n, nsurfaces)
{	xname = as.character (substitute (x) )

	if (missing (x) )
	{	nx = n [,which]
		n1 = nx [1]
		if (! all (n1 == nx) )
			stop (sprintf ("if %s missing, %s-dim needs to be same for all fv arrays", xname, xname) )
		x = rep (list (1:max (nx) ), nsurfaces)
	}
	else if (is.list (x) )
	{	if (nsurfaces == length (x) )
		{	y = vector ("list", nsurfaces)
			for (i in 1:nsurfaces)
			{	y [[i]] = .valx (x [[i]], xname="x")$x
				if (length (y [[i]]) != n [i, which])
					stop (sprintf ("%s [[%s]] not equal to %s-dim for fv or fv [[%s]]", xname, which, xname, which) )
			}
		}
		else
			stop (sprintf ("%s list, but length not equal to nsurfaces\n", xname) )
		x = y

	}
	else if (is.vector (x) )
	{	nx = n [,which]
		n1 = nx [1]
		if (! all (n1 == nx) )
			stop (sprintf ("if %s vector, %s-dim needs to be same, for all fv arrays", xname, xname) )
		x = .valx (x)$x
		if (n1 == length (x) )
			x = rep (list (x), nsurfaces)
		else
			stop (sprintf ("%s vector, and different to %s-dim size for fv arrays", xname, xname) )

	}
	else
		stop (sprintf ("%s needs to be numeric vector, or list of numeric vectors", xname) )
	x
}

.iso.val.array = function (fv)
{	fv = .as.numeric.array3 (fv)
	n = dim (fv)
	if (any (n < 10) )
		stop ("all fv array dims need to be >= 10")
	list (fv=fv, n=n)
}

.iso.val = function (x, y, z, fv, nsurfaces)
{	if (is.array (fv) )
	{	r = .iso.val.array (fv)
		n = matrix (rep (r$n, nsurfaces), nsurfaces, 3, byrow=TRUE)
		fv = rep (list (r$fv), nsurfaces)
	}
	else if (is.list (fv) )
	{	if (nsurfaces == length (fv) )
		{	n = matrix (0, nsurfaces, 3)
			for (i in 1:nsurfaces)
			{	r = .iso.val.array (fv [[i]])
				fv [[i]] = r$fv
				n [i,] = r$n
			}
		}
		else
			stop ("fv list, but length not equal nsurfaces")
	}
	else
		stop ("fv needs to be array, or list of arrays")

	x = .iso.x (1, x, n, nsurfaces)
	y = .iso.x (2, y, n, nsurfaces)
	z = .iso.x (3, z, n, nsurfaces)

	list (n=n, fv=fv, x=x, y=y, z=z)
}
	
.axis.scale.iso = function (x, xat, nsurfaces, xlim)
{	xname = as.character (substitute (x) )

	v = .axis.scale (x [[1]], xat, xname="x", xlim=xlim)
	x [[1]] = v$x
	xat = v$xat
	if (nsurfaces > 1)
	{	for (i in 2:nsurfaces)
			x [[i]] = .axis.scale (x [[i]], xname="x", xlim=xlim)$x
	}

	if (xname == "x") s = c (xname, "xat")
	else if (xname == "y") s = c (xname, "yat")
	else if (xname == "z") s = c (xname, "z0")
	else stop ()

	v = list (x, xat)
	names (v) = s
	v
}

.val.xlim = function (nsurfaces, x, reverse)
{	xlim = matrix (0, nsurfaces, 2)
	for (i in 1:nsurfaces)
		xlim [i,] = range (x [[i]])
	xlim = c (min (xlim), max (xlim) )
	if (reverse)
		xlim = rev (xlim)
	xlim
}
