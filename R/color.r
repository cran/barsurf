#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.colstr = function (with.alpha, colv)
{	if (with.alpha)
		rgb (colv [1], colv [2], colv [3], colv [4])
	else
		rgb (colv [1], colv [2], colv [3])
}

.mapcol = function (color.space, colvs)
{	m = ncol (colvs)
	if (m == 3)
		with.alpha = FALSE
	else if (m == 4)
		with.alpha = TRUE
	else
		stop ("cols needs 3 or 4 columns")
	if (color.space != "sRGB")
	{	if (color.space == "HCL")
		{	f = polarLUV
			colvs [,1:3] = colvs [,3:1]
		}
		else if (color.space %in% c ("XYZ", "RGB", "LAB", "polarLAB", "HSV", "HLS", "LUV", "polarLUV") )
			f = eval (parse (text = color.space) )
		else
			stop ("unsupported color space")
		for (i in 1:nrow (colvs) )
			colvs [i, 1:3] = coords (as (f (colvs [i, 1], colvs [i, 2], colvs [i, 3]), "sRGB") )
	}
	colvs [colvs < 0.001] = 0.001
	colvs [colvs > 0.999] = 0.999
	list (with.alpha, colvs)
}

set.bs.options = function (..., rendering.style="r", theme="blue")
{	if (! rendering.style %in% c ("r", "p", "e") )
		stop ("rendering.style not in {r, p, e}")
	bso = list ()
	bso$theme = theme
	bso$rendering.style = rendering.style
	if (theme == "heat")
	{	bso$soft.line.color = "#804040"
		bso$barface = "heat.barface"
		bso$litmus.fit = "heat.litmus.fit"
		bso$litmus.fit.hcv = "heat.litmus.fit"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "heat.litmus.fit"
		bso$litmus.fit.lum = "heat.litmus.fit.lum"
	}
	else if (theme == "gold")
	{	bso$soft.line.color = "#D0D000"
		bso$barface = "gold.barface"
		bso$litmus.fit = "blue.litmus.fit"
		bso$litmus.fit.hcv = "blue.litmus.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "blue.litmus.fit.flow"
		bso$litmus.fit.lum = "gold.litmus.fit.lum"
	}
	else if (theme == "blue")
	{	bso$soft.line.color = "#707088"
		bso$barface = "blue.barface"
		bso$litmus.fit = "blue.litmus.fit"
		bso$litmus.fit.hcv = "blue.litmus.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "blue.litmus.fit.flow"
		bso$litmus.fit.lum = "blue.litmus.fit.lum"
	}
	else if (theme == "green")
	{	bso$soft.line.color = "#608060"
		bso$barface = "green.barface"
		bso$litmus.fit = "green.litmus.fit"
		bso$litmus.fit.hcv = "green.litmus.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "green.litmus.fit.flow"
		bso$litmus.fit.lum = "green.litmus.fit.lum"
	}
	else if (theme == "purple")
	{	bso$soft.line.color = "#685068"
		bso$barface = "purple.barface"
		bso$litmus.fit = "blue.litmus.fit"
		bso$litmus.fit.hcv = "blue.litmus.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "blue.litmus.fit.flow"
		bso$litmus.fit.lum = "purple.litmus.fit.lum"
	}
	else
		stop ("unsupported theme")
	options (barsurf=bso)
}

set.bs.theme = function (theme)
{	rs = getOption ("barsurf")$rendering.style
	set.bs.options (rendering.style=rs, theme=theme)
}

rgb2hsv = function (colv)
	coords (as (sRGB (colv [1], colv [2], colv [3]), "HSV") )

hsv2rgb = function (colv)
	coords (as (HSV (colv [1], colv [2], colv [3]), "sRGB") )

rgb2hcl = function (colv)
	coords (as (sRGB (colv [1], colv [2], colv [3]), "polarLUV") )[3:1]

hcl2rgb = function (colv, correction=FALSE)
{	colv = coords (as (polarLUV (colv [3], colv [2], colv [1]), "sRGB") )
	if (correction)
	{	colv [colv < 0] = 0
		colv [colv > 1] = 1
	}
	colv
}
