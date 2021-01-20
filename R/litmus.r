#barsurf: Contour Plots, 3D Plots, Vector Fields and Heatmaps
#Copyright (C), Abby Spurdle, 2018 to 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.constf1 = function (cy)
{	f = function (x)
	{	. = .THAT ()
		y = rep (.$cy, length (x) )
		y [is.na (x)] = NA
		dim (y) = dim (x)
		y
	}
	.EXTEND (f,, cy=cy)
}

.constf2 = function (color.space, colvs, na.color)
{	colv = rbind (apply (colvs, 2, median) )
	colv = .map.color (colv, from=color.space, to="sRGB", correction=TRUE)
	if (length (colv) == 3) colstr = rgb (colv [1], colv [2], colv [3])
	else colstr = rgb (colv [1], colv [2], colv [3], colv [4])
	f = function (x)
	{	. = .THAT ()
		cols = rep (.$col, length (x) )
		cols [is.na (x)] = .$na.color
		dim (cols) = dim (x)
		cols
	}
	.EXTEND (f,, col=colstr, na.color=na.color)
}

.linear.interp = function (cx1, cx2, cy1, cy2)
{	f = function (x)
	{	. = .THAT ()
		w = (x - .$cx1) / (.$cx2 - .$cx1)
		y = (1 - w) * .$cy1 + w * .$cy2
		y [x < .$cx1] = .$cy1
		y [x > .$cx2] = .$cy2
		y
	}
	.EXTEND (f,, cx1=cx1, cx2=cx2, cy1=cy1, cy2=cy2)
}

litmus = function (a=0, b=1, colvs, ..., color.space="sRGB", na.color="#FFFFFF")
	litmus.spline (seq (a, b, length.out = nrow (colvs) ), colvs, color.space=color.space, na.color=na.color)

litmus.spline = function (cx, colvs, ..., color.space="sRGB", na.color="#FFFFFF")
{	n = length (cx)
	if (n < 2)
		stop ("needs 2 or more knots")
	if (n != nrow (colvs) )
		stop ("n != nrow (cols)")
	. = .valx (cx, xname="x")
	if (.$xtype == "-")
	{	cx = rev (.$x)
		colvs = colvs [n:1,]
	}
	colvs = .map.color (colvs, from=color.space, to="sRGB", correction=TRUE)
	colvs [colvs < 0.001] = 0.001
	colvs [colvs > 0.999] = 0.999
	if (ncol (colvs) == 3)
	{	f = .litmus.eval.3
		fa = NULL
	}
	else
	{	f = .litmus.eval.4
		fa = .litmus.ext (n, cx, colvs [,4])
	}
	.EXTEND (f, "litmus",
		a = min (cx), b = max (cx),
		fr = .litmus.ext (n, cx, colvs [,1]),
		fg = .litmus.ext (n, cx, colvs [,2]),
		fb = .litmus.ext (n, cx, colvs [,3]),
		fa=fa, na.color=na.color)
}

.litmus.ext = function (nc, cx, cy)
{	cy [cy < 0.001] = 0.001
	cy [cy > 0.999] = 0.999
	if (all (cy [1] == cy) )
		.constf1 (cy [1])
	else if (nc == 2)
		.linear.interp (cx [1], cx [nc], cy [1], cy [nc])
	else
	{	constraints = chs.constraints (bounds = c (0, 1) )
		chs (cx, cy, constraints=constraints, outside = c (cy [1], cy [nc]) )
	}
}

.litmus.eval.3 = function (x)
{	. = .THAT ()
	I = (! is.na (x) )
	y = x [I]
	cols = rep (.$na.color, length (x) )
	cols [I] = rgb (.$fr (y), .$fg (y), .$fb (y) )
	dim (cols) = dim (x)
	cols
}

.litmus.eval.4 = function (x)
{	. = .THAT ()
	I = (! is.na (x) )
	y = x [I]
	cols = rep (.$na.color, length (x) )
	cols [I] = rgb (.$fr (y), .$fg (y), .$fb (y), .$fa (y) )
	dim (cols) = dim (x)
	cols
}

mlitmus = function (..., default.color="#D0D0D0", na.color=default.color)
{	fs = list (...)
	n = length (fs)
	a = b = numeric (n)
	if (n == 0)
		stop ("mlitmus needs >= 1 litmus objects")
	for (i in 1:n)
	{	if (! inherits (fs [[i]], "litmus") )
			stop ("mlitmus needs litmus objects")
		a [i] = attr (fs [[i]], "a")
		b [i] = attr (fs [[i]], "b")
	}
	f = function (x)
	{	. = .THAT ()
		I = is.na (x)
		y = rep (.$default.color, length (x) )
		
		y [I] = .$na.color
		for (i in .$n:1)
		{	J = (! I & x >= .$a [i] & x <= .$b [i])
			I [J] = TRUE
			y [J] = .$color.functions [[i]](x [J])
		}
		dim (y) = dim (x)
		y
	}
	.EXTEND (f, "mlitmus",
		n=n, a=a, b=b,
		color.functions=fs,
		default.color=default.color, na.color=na.color)
}

litmus.fit = function (x, colvs, ..., color.space="sRGB", reverse=FALSE, equalize=0.85, na.color="#FFFFFF")
{	if (diff (range (x, na.rm=TRUE) ) == 0)
		.constf2 (color.space, colvs, na.color)
	else
	{	n = nrow (colvs)
		if (reverse)
			colvs = colvs [n:1,]
		if (n == 1)
			stop ("needs two or more colors")
		else if (n == 2 || equalize == 0)
		{	xlim = range (x, na.rm=TRUE)
			y = seq (xlim [1], xlim [2], length.out=n)
		}
		else if (equalize == 1)
		{	p = seq (0, 1, length.out=n)
			y = as.vector (quantile (x, p, na.rm=TRUE) )
		}
		else if (equalize > 0 && equalize < 1)
		{	xlim = range (x, na.rm=TRUE)
			y1 = seq (xlim [1], xlim [2], length.out=n)
			p = seq (0, 1, length.out=n)
			y2 = as.vector (quantile (x, p, na.rm=TRUE) )
			y = (1 - equalize) * y1 + equalize * y2
		}
		else
			stop ("equalize parameter not in [0, 1]")
		litmus.spline (y, colvs, color.space=color.space, na.color=na.color)
	}
}

print.litmus = function (x, ...)
{	. = attributes (x)
	cat ("litmus object, x in [", .$a, ", ", .$b, "]\n", sep="")
}

print.mlitmus = function (x, ...)
{	. = attributes (x)
	cat ("mlitmus object, x in [", min (.$a), ", ", max (.$b), "]\n", sep="")
}

plot.litmus = function (x, n=200, ...)
{	f = x

	p0 = par (mar = c (0.1, 0.1, 0.1, 0.1) )
	swatchplot (f (seq (min (attr (f, "a") ), max (attr (f, "b") ), length.out=n) ), sborder="#080808", border=NA, off=0)
	par (p0)
}

plot.mlitmus = function (x, n=200, ...)
	plot.litmus (x, n, ...)
