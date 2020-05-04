#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

heat.barface = function () barface (c (20, 65, 60, 0.95), c (20, 55, 67.5, 0.85), color.space="HCL")
gold.barface = function () barface (c (77, 77, 70, 0.95), c (77, 70, 77, 0.85), color.space="HCL")
blue.barface = function () barface (c (220, 30, 67.5, 0.95), c (220, 30, 75, 0.85), color.space="HCL")
green.barface = function () barface (c (137.5, 60, 70, 0.95), c (137.5, 55, 75, 0.85), color.space="HCL")
purple.barface = function () barface (c (285, 17.5, 65, 0.95), c (285, 17.5, 72.5, 0.85), color.space="HCL")

heat.litmus = function (a=0, b=1, ..., reverse=FALSE) heat.litmus.fit (c (a, b), reverse=reverse)
blue.litmus = function (a=0, b=1, ..., reverse=FALSE) blue.litmus.fit (c (a, b), reverse=reverse)
green.litmus = function (a=0, b=1, ..., reverse=FALSE) green.litmus.fit (c (a, b), reverse=reverse)
blue.litmus.hcv = function (a=0, b=1, ..., reverse=FALSE) blue.litmus.fit.hcv (c (a, b), reverse=reverse)
green.litmus.hcv = function (a=0, b=1, ..., reverse=FALSE) green.litmus.fit.hcv (c (a, b), reverse=reverse)
blue.litmus.flow = function (a=0, b=1, ..., reverse=FALSE) blue.litmus.fit.flow (c (a, b), reverse=reverse)
green.litmus.flow = function (a=0, b=1, ..., reverse=FALSE) green.litmus.fit.flow (c (a, b), reverse=reverse)

hot.and.cold = function (a=-1, b=1, xb=0) hot.and.cold.fit (c (a, b), xb)

rainbow.litmus = function (a=0, b=1, ..., c=42.5, l=75,  start=65, end=315)
	rainbow.litmus.fit (c (a, b), c=c, l=l, start=start, end=end, equalize=0)
rainbow.litmus.2 = function (a=0, b=1, ..., c=50, l=70, start=0, end=360)
	rainbow.litmus.fit (c (a, b), c=c, l=l, start=start, end=end, equalize=0)

glass.rainbow = function (a=0, b=1, alpha=0.3, ..., c=42.5, l=62.5, start=42.5, end=260)
	glass.rainbow.fit (c (a, b), alpha, c=c, l=l, start=start, end=end, equalize=0)

heat.litmus.fit = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		1, 0, 0,
		1, 0.40, 0.0,
		1, 0.65, 0.0,
		1, 0.9, 0.275,
		1, 1, 1),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="sRGB", reverse=reverse, equalize=equalize)
}

blue.litmus.fit = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = cbind (c (180, 210, 220, 275), 35, 75)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

green.litmus.fit = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = cbind (c (170, 140, 130, 95), 35, 75)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

blue.litmus.fit.hcv = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		220, 50, 45,
		220, 50, 55,
		220, 50, 65,
		210, 50, 75,
		210, 12.5, 85,
		210, 0, 90),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

green.litmus.fit.hcv = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		140, 65, 50,
		130, 65, 60,
		120, 65, 70,
		110, 65, 80,
		100, 12.5, 90,
		90, 0, 95),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

blue.litmus.fit.flow = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		220, 40, 55,
		0, 0, 90),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

green.litmus.fit.flow = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		140, 40, 55,
		0, 0, 90),, 3, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

heat.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		15, 80, 50, 0.875,
		0, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

gold.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		77, 65, 65, 0.875,
		77, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

blue.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		220, 30, 57.5, 0.875,
		0, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

green.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
{	colvs = matrix (c (
		137.5, 30, 57.5, 0.875,
		0, 0, 85, 0.875),, 4, byrow=TRUE)
	litmus.fit (x, colvs, color.space="HCL", reverse=reverse, equalize=equalize)
}

purple.litmus.fit.lum = function (x, ..., reverse=FALSE, equalize=0.85)
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

rainbow.litmus.fit = function (x, ..., c=42.5, l=75, start=65, end=315, equalize=0.85)
{	colvs = seq (start, end, length.out=6)
	colvs = cbind (colvs, c, l)
	litmus.fit (x, colvs, color.space="HCL", equalize=equalize)
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
	litmus.fit (x, colvs, color.space="HCL", equalize=equalize)
}
