#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.colf = function (colvs)
	map.color (colvs, TRUE, from="HCL")

.tcol = function (colvs) .colf (colvs [1:4])
.scol = function (colvs) .colf (colvs [5:8])
.icol = function (colvs) .colf (.colm ("HCL", colvs, nc=4)$colvs)

.predef = function (a, b, colvs, color.space, reverse=FALSE)
{	if (reverse) litmus (b, a, colvs, color.space=color.space)
	else litmus (a, b, colvs, color.space=color.space)
}

.predef.fit = function (x, colvs, color.space, reverse=FALSE, equalize=0)
	litmus.fit (x, colvs, color.space=color.space, reverse=reverse, equalize=equalize)

map.color = function (colvs, string=FALSE, ..., from="sRGB", to="HCL", correction=TRUE)
{	colvs = rbind (colvs)
	nc = ncol (colvs)
	rownames (colvs) = NULL
	names = rep ("", nc)
	if (nc == 3) 0
	else if (nc == 4) names [4] = "a"
	else
		stop ("colvs needs 3 or 4 columns")
	if (string)
		to="sRGB"
	color.spaces = c ("XYZ", "RGB", "sRGB", "LAB", "polarLAB", "HSV", "HLS", "LUV", "polarLUV", "HCL")
	if (! (from %in% color.spaces && to %in% color.spaces) )
		stop ("unsupported color space")
	if (from != to)
	{	if (from == "HCL")
		{	from = polarLUV
			colvs [,1:3] = colvs [,3:1]
		}
		else
			from = eval (str2lang (from) )
		if (to == "HCL")
		{	v = coords (as (from (colvs [,1], colvs [,2], colvs [,3]), "polarLUV") )
			colvs [,3:1] = v
			names [1:3] = c ("H", "C", "L")
		}
		else
		{	v = coords (as (from (colvs [,1], colvs [,2], colvs [,3]), to) )
			colvs [,1:3] = v
			names [1:3] = colnames (v)
		}
		colnames (colvs) = names
		if (to == "sRGB" && correction)
		{	colvs [colvs < 0] = 0
			colvs [colvs > 1] = 1
		}
	}
	if (string)
	{	if (nc == 3)
			rgb (colvs [,1], colvs [,2], colvs [,3])
		else
			rgb (colvs [,1], colvs [,2], colvs [,3], colvs [,4])
	}
	else
		colvs
}

map.color.3 = function (x, y, z, alpha, string=FALSE, ..., from="sRGB", to="HCL", correction=TRUE)
{	if (missing (alpha) ) colvs = cbind (x, y, z)
	else colvs = cbind (x, y, z, alpha)
	map.color (colvs, string, from=from, to=to, correction=correction)
}
