#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.constf = function (color.space, colvs, na.col)
{	colv = rbind (apply (colvs, 2, median) )
	v = .mapcol (color.space, colv)
	with.alpha = v [[1]]
	colv = v [[2]]
	f = function (x)
	{	. = .THAT ()
		cols = rep (.$col, length (x) )
		cols [is.na (x)] = .$na.col
		dim (cols) = dim (x)
		cols
	}
	class (f) = c ("constant.color", class (f) )
	attr (f, "col") = .colstr (with.alpha, colv)
	attr (f, "na.col") = na.col
	f
}

barface = function (coltv, colfv, color.space="sRGB")
{	v = .mapcol (color.space, rbind (coltv) )
	with.alpha = v [[1]]
	coltv = v [[2]]
	if (missing (colfv) )
		colfv = (coltv + 0.999) / 2
	else
	{	v = .mapcol (color.space, rbind (colfv) )
		if (with.alpha != v [[1]])
			stop ("color vectors different lengths")
		colfv = v [[2]]
	}
	f = function (x)
	{	. = .THAT ()
		cols = rep (.$colf, length (x) )
		cols [x] = .$colt
		dim (cols) = dim (x)
		cols
	}
	class (f) = c ("barface", class (f) )
	attr (f, "colt") = colt = .colstr (with.alpha, coltv)
	attr (f, "colf") = colf = .colstr (with.alpha, colfv)
	f
}

litmus = function (a=0, b=1, colvs, color.space="sRGB", na.col="#FFFFFF")
	litmus.spline (seq (a, b, length.out = nrow (colvs) ), colvs, color.space, na.col)

litmus.spline = function (x, colvs, color.space="sRGB", na.col="#FFFFFF")
{	n = length (x)
	if (n < 2)
		stop ("needs 2 or more knots")
	if (n != nrow (colvs) )
		stop ("n != nrow (cols)")
	v = .mapcol (color.space, colvs)
	with.alpha = v [[1]]
	colvs = v [[2]]
	if (with.alpha)
		f = .litmus.eval.4
	else
		f = .litmus.eval.3
	class (f) = c ("litmus", class (f) )
	attr (f, "a") = min (x)
	attr (f, "b") = max (x)
	attr (f, "fr") = .litmus.ext (n, x, colvs [,1])
	attr (f, "fg") = .litmus.ext (n, x, colvs [,2])
	attr (f, "fb") = .litmus.ext (n, x, colvs [,3])
	if (with.alpha)
		attr (f, "fa") = .litmus.ext (n, x, colvs [,4])
	attr (f, "na.col") = na.col
	f
}

.litmus.ext = function (nc, cx, cy)
{	cy [cy < 0.001] = 0.001
	cy [cy > 0.999] = 0.999
	if (nc == 2)
	{	f = function (x)
		{	. = .THAT ()
			w = (x - .$cx [1]) / (.$cx [2] - .$cx [1])
			y = (1 - w) * .$cy [1] + w * .$cy [2]
			y [x < .$cx [1] ] = .$cy [1]
			y [x > .$cx [2] ] = .$cy [2]
			y
		}
		class (f) = c ("litmus", class (f) )
		attr (f, "cx") = cx
		attr (f, "cy") = cy
		f
	}
	else
	{	cb = chs.slopes (cx, cy)
		f = chs (cx, cy, cb, outside = c (cy [1], cy [nc]) )
		a = chs.argmins (f, warning=FALSE)
		b = chs.argmaxs (f, warning=FALSE)
		if (any (f (a) < 0.001) || any (f (b) > 0.999) )
		{	cb = rep (0, nc)
			f = chs (cx, cy, cb, outside = c (cy [1], cy [nc]))
		}
		f
	}
}

.litmus.eval.3 = function (x)
{	. = .THAT ()
	I = (! is.na (x) )
	y = x [I]
	cols = rep (.$na.col, length (x) )
	cols [I] = rgb (.$fr (y), .$fg (y), .$fb (y) )
	dim (cols) = dim (x)
	cols
}

.litmus.eval.4 = function (x)
{	. = .THAT ()
	I = (! is.na (x) )
	y = x [I]
	cols = rep (.$na.col, length (x) )
	cols [I] = rgb (.$fr (y), .$fg (y), .$fb (y), .$fa (y) )
	dim (cols) = dim (x)
	cols
}

mlitmus = function (..., default.col="#D0D0D0", na.col=default.col)
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
		y = rep (.$default.col, length (x) )
		I = is.na (x)
		y [I] = .$na.col
		for (i in .$n:1)
		{	J = (! I & x >= .$a [i] & x <= .$b [i])
			I [J] = TRUE
			y [J] = .$color.functions [[i]](x [J])
		}
		dim (y) = dim (x)
		y
	}
	class (f) = c ("mlitmus", class (f) )
	attr (f, "n") = n
	attr (f, "a") = a
	attr (f, "b") = b
	attr (f, "color.functions") = fs
	attr (f, "default.col") = default.col
	attr (f, "na.col") = na.col
	f
}



print.barface = function (x, ...)
	cat ("barface object\n")

print.litmus = function (x, ...)
{	. = attributes (x)
	cat ("litmus object, x in [", .$a, ", ", .$b, "]\n", sep="")
}

print.mlitmus = function (x, ...)
{	. = attributes (x)
	cat ("mlitmus object, x in [", min (.$a), ", ", max (.$b), "]\n", sep="")
}

plot.barface = function (x, ...)
{	f = x

	p0 = par (mar = c (0.1, 0.1, 0.1, 0.1) )
	swatchplot (c (f (TRUE), f (FALSE) ), sborder="#080808", border="#080808", off=0)
	par (p0)
}

plot.litmus = function (x, n=200, ...)
{	f = x

	p0 = par (mar = c (0.1, 0.1, 0.1, 0.1) )
	swatchplot (f (seq (min (attr (f, "a") ), max (attr (f, "b") ), length.out=n) ), sborder="#080808", border=NA, off=0)
	par (p0)
}

plot.mlitmus = function (x, n=200, ...)
	plot.litmus (x, n, ...)

barface.heat = function () barface (c (20, 65, 60, 0.95), c (20, 55, 67.5, 0.85), "HCL")
barface.gold = function () barface (c (77, 77, 70, 0.95), c (77, 70, 77, 0.85), "HCL")
barface.blue = function () barface (c (220, 30, 67.5, 0.95), c (220, 30, 75, 0.85), "HCL")
barface.green = function () barface (c (137.5, 60, 70, 0.95), c (137.5, 55, 75, 0.85), "HCL")
barface.purple = function () barface (c (285, 17.5, 65, 0.95), c (285, 17.5, 72.5, 0.85), "HCL")

litmus.heat = function (a=0, b=1, ..., reverse=FALSE) litmus.heat.fit (c (a, b), reverse=reverse)
litmus.blue = function (a=0, b=1, ..., reverse=FALSE) litmus.blue.fit (c (a, b), reverse=reverse)
litmus.green = function (a=0, b=1, ..., reverse=FALSE) litmus.green.fit (c (a, b), reverse=reverse)
litmus.blue.hcv = function (a=0, b=1, ..., reverse=FALSE) litmus.blue.fit.hcv (c (a, b), reverse=reverse)
litmus.green.hcv = function (a=0, b=1, ..., reverse=FALSE) litmus.green.fit.hcv (c (a, b), reverse=reverse)
litmus.blue.flow = function (a=0, b=1, ..., reverse=FALSE) litmus.blue.fit.flow (c (a, b), reverse=reverse)
litmus.green.flow = function (a=0, b=1, ..., reverse=FALSE) litmus.green.fit.flow (c (a, b), reverse=reverse)

hot.and.cold = function (a=-1, b=1, xb=0) hot.and.cold.fit (c (a, b), xb)

litmus.rainbow = function (a=0, b=1, ..., c=42.5, l=75,  start=65, end=315)
	litmus.rainbow.fit (c (a, b), c=c, l=l, start=start, end=end, equalize=0)
litmus.rainbow.2 = function (a=0, b=1, ..., c=50, l=70, start=0, end=360)
	litmus.rainbow.fit (c (a, b), c=c, l=l, start=start, end=end, equalize=0)

glass.rainbow = function (a=0, b=1, alpha=0.3, ..., c=42.5, l=62.5, start=42.5, end=260)
	glass.rainbow.fit (c (a, b), alpha, c=c, l=l, start=start, end=end, equalize=0)

litmus.fit = function (x, colvs, ..., color.space="sRGB", reverse=FALSE, equalize=0.85, na.col="#FFFFFF")
{	if (diff (range (x, na.rm=TRUE) ) == 0)
		.constf (color.space, colvs, na.col)
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
		litmus.spline (y, colvs, color.space, na.col)
	}
}

litmus.heat.fit = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		1, 0, 0,
		1, 0.40, 0.0,
		1, 0.65, 0.0,
		1, 0.9, 0.275,
		1, 1, 1),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="sRGB", reverse=reverse, equalize=equalize)
}

litmus.blue.fit = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = cbind (c (180, 210, 220, 275), 35, 75)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.green.fit = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = cbind (c (170, 140, 130, 95), 35, 75)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.blue.fit.hcv = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		220, 50, 45,
		220, 50, 55,
		220, 50, 65,
		210, 50, 75,
		210, 12.5, 85,
		210, 0, 90),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.green.fit.hcv = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		140, 65, 50,
		130, 65, 60,
		120, 65, 70,
		110, 65, 80,
		100, 12.5, 90,
		90, 0, 95),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.blue.fit.flow = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		220, 40, 55,
		0, 0, 90),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.green.fit.flow = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		140, 40, 55,
		0, 0, 90),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.heat.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		15, 80, 50, 0.875,
		0, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.gold.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		77, 65, 65, 0.875,
		77, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.blue.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		220, 30, 57.5, 0.875,
		0, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.green.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		137.5, 30, 57.5, 0.875,
		0, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

litmus.purple.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		285, 30, 57.5, 0.875,
		0, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

hot.and.cold.fit = function (x, xb=0)
{	x1 = c (x [x < xb], xb)
	x2 = c (xb, x [x >= xb])
	colvs = matrix (c (
		240, 40, 30,
		240, 60, 40,
		240, 60, 60,
		240, 60, 40,
		240, 40, 30),, 3, byrow=TRUE)
	f1 = litmus.fit (x1, colvs, color.space="HCL", equalize=0)
	colvs = matrix (c (
		15, 40, 30,
		15, 60, 40,
		15, 60, 60,
		15, 60, 40,
		15, 40, 30),, 3, byrow=TRUE)
	f2 = litmus.fit (x2, colvs, color.space="HCL", equalize=0)
	mlitmus (f1, f2)
}

litmus.rainbow.fit = function (x, ..., c=42.5, l=75, start=65, end=315, equalize=0.85)
{	colvs = seq (start, end, length.out=6)
	colvs = cbind (colvs, c, l)
	litmus.fit (x, colvs, color.space="HCL", equalize=equalize)
}

litmus.rainbow.fit.2 = function (x, ..., c=50, l=70, start=0, end=360, equalize=0.85)
	litmus.rainbow.fit (x, c=c, l=l, start=start, end=end, equalize=equalize)

glass.rainbow.fit = function (x, alpha=0.3, ..., c=42.5, l=62.5, start=42.5, end=260, equalize=0.85)
{	n = length (alpha)
	if (n == 0)
		stop ("length (alpha) == 0")
	else if (n == 1)
	{	colvs = seq (start, end, length.out=6)
		colvs = cbind (colvs, c, l, alpha)
	}
	else
	{	colvs = seq (start, end, length.out=n)
		colvs = cbind (colvs, c, l, alpha)
	}
	litmus.fit (x, colvs, color.space="HCL", equalize=equalize)
}
