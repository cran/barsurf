#barsurf: Contour Plots, 3D Plots, Vector Fields and Heatmaps
#Copyright (C), Abby Spurdle, 2018 to 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

########################################
#simple palettes
########################################
gold.top.color = function () .tcol (.gold.bar)
blue.top.color = function () .tcol (.blue.bar)
green.top.color = function () .tcol (.green.bar)
purple.top.color = function () .tcol (.purple.bar)
heat.top.color = function () .tcol (.heat.bar)

gold.side.color = function () .scol (.gold.bar)
blue.side.color = function () .scol (.blue.bar)
green.side.color = function () .scol (.green.bar)
purple.side.color = function () .scol (.purple.bar)
heat.side.color = function () .scol (.heat.bar)

gold.iso.colors = function () .icol (.gold.iso)
blue.iso.colors = function () .icol (.blue.iso)
green.iso.colors = function () .icol (.green.iso)
purple.iso.colors = function () .icol (.purple.iso)
heat.iso.colors = function () .icol (.heat.iso)

########################################
#litmus objects
########################################
gold.litmus = function (a=0, b=1) .predef (a, b, .gold$colvs, .gold$color.space)
blue.litmus = function (a=0, b=1) .predef (a, b, .blue$colvs, .blue$color.space)
green.litmus = function (a=0, b=1) .predef (a, b, .green$colvs, .green$color.space)
heat.litmus = function (a=0, b=1) .predef (a, b, .heat$colvs, .heat$color.space)
blue.litmus.hcv = function (a=0, b=1) .predef (a, b, .blue.hcv$colvs, .blue.hcv$color.space)
green.litmus.hcv = function (a=0, b=1) .predef (a, b, .green.hcv$colvs, .green.hcv$color.space)
blue.litmus.flow = function (a=0, b=1) .predef (a, b, .blue.flow$colvs, .blue.flow$color.space)
green.litmus.flow = function (a=0, b=1) .predef (a, b, .green.flow$colvs, .green.flow$color.space)

rainbow.litmus = function (a=0, b=1, ..., c=42.5, l=75,  start=65, end=315)
{	colvs = seq (start, end, length.out=6)
	.predef (a, b, cbind (colvs, c, l), "HCL")
}

rainbow.litmus.2 = function (a=0, b=1, ..., c=50, l=70, start=0, end=360)
	rainbow.litmus (a, b, c=c, l=l, start=start, end=end, equalize=0)

glass.litmus = function (a=0, b=1, alpha=0.3, ..., c=42.5, l=62.5, start=42.5, end=260)
{	if (b < a)
	{	u = start
		start = end
		end = u
	}
	glass.rainbow.fit (c (a, b), alpha, c=c, l=l, start=start, end=end, equalize=0)
}

hot.and.cold = function (t=0, d=1, ..., hot.hue=35, cold.hue=255)
	hot.and.cold.fit (c (t - d, t + d), t=t, hot.hue=hot.hue, cold.hue=cold.hue)

########################################
#litmus-fitting functions
########################################
gold.litmus.fit = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .gold$colvs, .gold$color.space, reverse, equalize)
blue.litmus.fit = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .blue$colvs, .blue$color.space, reverse, equalize)
green.litmus.fit = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .green$colvs, .green$color.space, reverse, equalize)
heat.litmus.fit = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .heat$colvs, .heat$color.space, reverse, equalize)

blue.litmus.fit.hcv = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .blue.hcv$colvs, .blue.hcv$color.space, reverse, equalize)
green.litmus.fit.hcv = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .green.hcv$colvs, .green.hcv$color.space, reverse, equalize)

blue.litmus.fit.flow = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .blue.flow$colvs, .blue.flow$color.space, reverse, equalize)
green.litmus.fit.flow = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .green.flow$colvs, .green.flow$color.space, reverse, equalize)

gold.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .gold.lum$colvs, .gold.lum$color.space, reverse, equalize)
blue.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .blue.lum$colvs, .blue.lum$color.space, reverse, equalize)
green.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .green.lum$colvs, .green.lum$color.space, reverse, equalize)
purple.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .purple.lum$colvs, .purple.lum$color.space, reverse, equalize)
heat.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
	.predef.fit (x, .heat.lum$colvs, .heat.lum$color.space, reverse, equalize)

rainbow.litmus.fit = function (x, ..., c=42.5, l=75, start=65, end=315, equalize=0.85)
{	colvs = seq (start, end, length.out=6)
	colvs = cbind (colvs, c, l)
	.predef.fit (x, colvs, "HCL", FALSE, equalize)
}

rainbow.litmus.fit.2 = function (x, ..., c=50, l=70, start=0, end=360, equalize=0.85)
	rainbow.litmus.fit (x, c=c, l=l, start=start, end=end, equalize=equalize)

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
	.predef.fit (x, colvs, "HCL", FALSE, equalize)
}

hot.and.cold.fit = function (x, ..., t=0, symetric=TRUE, hot.hue=35, cold.hue=255, equalize=0.85)
{	n = length (.hc.chroma)
	I = c (n, n)

	cold = cbind (cold.hue, .hc.chroma, .hc.lum)
	warm = cbind (c (cold.hue, hot.hue), .hc.chroma [I], .hc.lum [I]) 
	hot = cbind (hot.hue, rev (.hc.chroma), rev (.hc.lum) )

	t = sort (t)
	if (length (t) == 1)
	{	x1 = c (x [x <= t], t)
		x2 = c (t, x [x >= t])
		if (symetric)
		{	x1 = c (x1, 2 * t - x2)
			x2 = c (x2, 2 * t - x1)
		}
		if (length (x1) < 2 || length (x2) < 2)
				stop ("needs one or more points, on each side")
		f1 = .predef.fit (x1, cold, "HCL", FALSE, equalize)
		f2 = .predef.fit (x2, hot, "HCL", FALSE, equalize)
		mlitmus (f1, f2)
	}
	else if (length (t) == 2)
	{	x1 = c (x [x <= t [1] ], t [1] )
		x2 = c (t [1], x [x >= t [1] & x <= t [2] ], t [2])
		x3 = c (t [2], x [x >= t [2] ])
		if (symetric)
		{	sumt = sum (t)
			x1 = c (x1, sumt - x3)
			x3 = c (x3, sumt - x1)
		}
		f1 = .predef.fit (x1, cold, "HCL", FALSE, equalize)
		f2 = .predef.fit (x2, warm, "HCL", FALSE, equalize)
		f3 = .predef.fit (x3, hot, "HCL", FALSE, equalize)
		mlitmus (f1, f2, f3)
	}
	else
		stop ("length of t needs to be one or two")
}

########################################
#other
########################################
gold.seq = function (n, ...)
	gold.litmus (1, n, ...)(1:n)
blue.seq = function (n, ..., hcv=FALSE)
{	if (hcv) blue.litmus.hcv (1, n, ...)(1:n)
	else blue.litmus (1, n, ...)(1:n)
}
green.seq = function (n, ..., hcv=FALSE)
{	if (hcv) green.litmus.hcv (1, n, ...)(1:n)
	else green.litmus (1, n, ...)(1:n)
}
rainbow.seq = function (n, ...) rainbow.litmus (1, n, ...)(1:n)
heat.seq = function (n, ...) heat.litmus (1, n, ...)(1:n)

gold.fit = function (x, ...)
	gold.litmus.fit (x, ...)(x)
blue.fit = function (x, ..., hcv=FALSE)
{	if (hcv) blue.litmus.fit.hcv (x, ...)(x)
	else blue.litmus.fit (x, ...)(x)
}
green.fit = function (x, ..., hcv=FALSE)
{	if (hcv) green.litmus.fit.hcv (x, ...)(x)
	else green.litmus.fit (x, ...)(x)
}
rainbow.fit = function (x, ...) rainbow.litmus.fit (x, ...)(x)
heat.fit = function (x, ...) heat.litmus.fit (x, ...)(x)
