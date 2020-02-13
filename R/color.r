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
	{	bso$soft.line.col = "#804040"
		bso$barface = "barface.heat"
		bso$litmus.fit = "litmus.heat.fit"
		bso$litmus.fit.hcv = "litmus.heat.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "litmus.heat.fit"
		bso$litmus.fit.lum = "litmus.heat.fit.lum"
	}
	else if (theme == "gold")
	{	bso$soft.line.col = "#D0D000"
		bso$barface = "barface.gold"
		bso$litmus.fit = "litmus.blue.fit"
		bso$litmus.fit.hcv = "litmus.blue.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "litmus.blue.fit.flow"
		bso$litmus.fit.lum = "litmus.gold.fit.lum"
	}
	else if (theme == "blue")
	{	bso$soft.line.col = "#707088"
		bso$barface = "barface.blue"
		bso$litmus.fit = "litmus.blue.fit"
		bso$litmus.fit.hcv = "litmus.blue.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "litmus.blue.fit.flow"
		bso$litmus.fit.lum = "litmus.blue.fit.lum"
	}
	else if (theme == "green")
	{	bso$soft.line.col = "#608060"
		bso$barface = "barface.green"
		bso$litmus.fit = "litmus.green.fit"
		bso$litmus.fit.hcv = "litmus.green.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "litmus.green.fit.flow"
		bso$litmus.fit.lum = "litmus.green.fit.lum"
	}
	else if (theme == "purple")
	{	bso$soft.line.col = "#685068"
		bso$barface = "barface.purple"
		bso$litmus.fit = "litmus.blue.fit"
		bso$litmus.fit.hcv = "litmus.blue.fit.hcv"
		bso$litmus.fit.glass = "glass.rainbow.fit"
		bso$litmus.fit.flow = "litmus.blue.fit.flow"
		bso$litmus.fit.lum = "litmus.purple.fit.lum"
	}
	else
		stop ("unsupported theme")
	options (barsurf=bso)
}

set.bs.theme = function (theme)
{	rs = getOption ("barsurf")$rendering.style
	set.bs.options (rendering.style=rs, theme=theme)
}

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
