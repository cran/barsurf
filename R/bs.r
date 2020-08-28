#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.dbl = function (x) rep_len (x, 2)
.trpl = function (x) rep_len (x, 3)

.intseq = function (n, a, b)
{	x = seq (a, b, length.out=n)
	as.integer (round (x) )
}

#intoo derivative
.EXTEND = function (object, class=NULL, ...)
{	class = c (class, class (object) )
	structure (object, class=class, ...)
}

#intoo derivative
.THAT = function () 
{	this = sys.function (-1)
	attributes (this)
}

#intoo derivative
.UNPACK = function (x)
{	list2env (x, parent.frame (1) )
	invisible (NULL)
}

.catchargs = function (..., .panel.lines, arrows, cols, z.reverse, reverse.z)
{	if (getOption ("barsurf")$test.mode)
	{	v = list (...)
		if (length (v) > 0)
		{	s = names (v)
			s [s == ""] = "unnamed arg"
			f = as.character (sys.call (-1)[1])
		
			s = paste (s, collapse=", ")
			s = paste0 ("\n    unmatched arguments:\n        ", s, "\n    in call to *", f, "*\n") 
			stop (s)
		}
	}
	invisible (NULL)
}

.ST = function (colf, colff, fv, hcv=NULL, type=NULL, ...)
{	if (missing (colf) )
	{	if (missing (colff) )
		{	if (is.null (type) )
			{	if (hcv) colff = opt.litmus.fit.hcv ()
				else colff = opt.litmus.fit ()
			}
			else
				colff = .st (type, TRUE)
		}
		colf = colff (fv, ...)
	}
	else
		colf
}

.mar3 = function (nhl)
	par (mar=c (0.2, 0.2, nhl + 0.5, 0.2) )

matrix.margins = function ()
{	mar = par ("mar")
	par (mar = mar [c (3, 2, 1, 4) ])
}


.as.numeric.matrix = function (fv)
{	fv = as.matrix (fv)
	mode (fv) = "numeric"
	fv
}

.as.numeric.array3 = function (fv)
{	fv = as.array (fv)
	if (length (dim (fv) ) != 3)
		stop ("needs array, 3d")
	mode (fv) = "numeric"
	fv
}

.is.reverse = function (xlim, ylim=0:1)
	c (xlim [1] > xlim [2], ylim [1] > ylim [2])

.is.maximal = function (fv)
{	dims = dim (fv)
	nx = dims [1]
	ny = dims [2]
	nz = dims [3]

	g = numeric (8)
	g [1] = fv [2, 2, 2] - fv [1, 1, 1]
	g [2] = fv [2, 2, nz - 1] - fv [1, 1, nz]
	g [3] = fv [2, ny - 1, 2] - fv [1, ny, 1]
	g [4] = fv [2, ny - 1, nz - 1] - fv [1, ny, nz]
	g [5] = fv [nx - 1, 2, 2] - fv [nx, 1, 1]
	g [6] = fv [nx - 1, 2, nz - 1] - fv [nx, 1, nz]
	g [7] = fv [nx - 1, ny - 1, 2] - fv [nx, ny, 1]
	g [8] = fv [nx - 1, ny - 1, nz - 1] - fv [nx, ny, nz]
	mean (g) >= 0
} 

.outer.3 = function (f, n, x, y, z, ...)
{	x2 = rep (x, times = n [2] * n [3])
	y2 = rep (y, each = n [1], times = n [3])
	z2 = rep (z, each = n [1] * n [2])
	fv = f (x2, y2, z2, ...)
	array (fv, n)
}

.outer.dxdy = function (f, n, x, y)
{	x = rep (x, times = n [2])
	y = rep (y, each = n [1])
	v = f (x, y)
	dx = matrix (v [,1], n [1], n [2])
	dy = matrix (v [,2], n [1], n [2])
	list (dx, dy)
}

.outer.dz = function (f, n, x, y, z)
{	x = rep (x, times = n [2] * n [3])
	y = rep (y, each = n [1], times = n [3])
	z = rep (z, each = n [1] * n [2])
	v = f (x, y, z)
	dx = array (v [,1], n)
	dy = array (v [,2], n)
	dz = array (v [,3], n)
	list (dx, dy, dz)
}

#deprecated
plotf_cfield_3d = function (..., reverse.z=FALSE)
	plotf_cfield3 (..., z.reverse=reverse.z)
barface = function ()
{	f = function (top)
	{	if (top) opt.top.color ()
		else opt.side.color ()
	}
}
litmus.rainbow.fit = function (...) rainbow.litmus.fit (...)

#not deprecated
#but includes deprecated args: arrows, cols
.extract.private.args = function (..., .panel.lines, arrows, cols)
{	if (missing (.panel.lines) ) .panel.lines = NULL
	if (missing (arrows) ) arrows = NULL
	if (missing (cols) ) cols = NULL
	list (panel.lines=.panel.lines, arrows=arrows, cols=cols)
}

#example function
rotated.sinc = function (x, y)
{	r = sqrt (x^2 + y^2)
	fv = sin (r) / r
	fv [is.nan (fv)] = 1
	fv
}

#example function
bispherical.dist = function (x, y, z)
{	r1 = sqrt ( (x + 1)^2 + (y - 1)^2 + z^2)
	r2 = sqrt ( (x - 1)^2 + (y + 1)^2 + z^2)
	I = r2 < r1
	r1 [I] = r2 [I]
	r1
}

.concf = function (x, y, r=1, k=1)
{	theta = atan2 (x, y) - pi / 2
	r = 1 - abs (r - sqrt (x^2 + y^2) )
	r [r < 0] = 0
	r = r ^ k
	dx = r * sin (theta)
	dy = r * cos (theta)
	cbind (dx=dx, dy=dy, r=r)
}

#example function
circular.field = function (x, y)
	.concf (x, y)[,1:2, drop=FALSE]

#example function
plughole.field = function (x, y, z)
{	vf = .concf (x, y, abs (z), 8)
	u = 1 - abs (z)
	u [u < 0] = 0
	dz = -1 * u * as.vector (vf [,3])
	cbind (vf [,1:2, drop=FALSE], dz=dz)
}
