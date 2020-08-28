#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.val.theme = function (theme)
{	if (! theme %in% c ("gold", "blue", "green", "purple", "heat") )
		stop ("not valid theme")
}

.val.type = function (theme)
{	if (! theme %in% c ("main", "hcv", "glass", "flow", "lum") )
		stop ("not valid litmus type")
}

.sb0 = function (type, s)
{	.val.theme (s)

	str = paste0 (s, ".", type)
	eval (str2lang (str) )()
}

.sgrid0 = function (s)
{	.val.theme (s)

	str = paste0 (".sgrid.colors")
	as.vector (eval (str2lang (str) )[s])
}

.st0 = function (s, type="main", ret.str=FALSE)
{	.val.theme (s)
	.val.type (type)

	str = paste0 (".s", s)
	v = eval (str2lang (str) )[[type]]
	str = paste0 (v [1], ".litmus.fit")
	if (length (v) == 2)
		str = paste0 (str, ".", v [2])
	if (ret.str)
		str
	else
		eval (str2lang (str) )
}

set.bs.options = function (...,
	nhl=1, ref.arrows=TRUE,
	rendering.style="R", theme="blue",
	top.color, side.color, sgrid.color, iso.colors,
	main, hcv, glass="glass.rainbow.fit", flow, lum,
	test.mode=FALSE)
{	rendering.style =  tolower (rendering.style)
	if (! rendering.style %in% c ("r", "pdf", "e") )
		stop ("rendering.style not R or pdf}")
	if (rendering.style == "e") rendering.style = "pdf"

	.val.theme (theme)

	if (missing (top.color) ) top.color = st.top.color (theme)
	if (missing (side.color) ) side.color = st.side.color (theme)
	if (missing (sgrid.color) ) sgrid.color = .sgrid.colors [theme]
	if (missing (iso.colors) ) iso.colors = st.iso.colors (theme)
	if (missing (main) ) main = .st0 (theme, "main", TRUE)
	if (missing (hcv) ) hcv = .st0 (theme, "hcv", TRUE)
	if (missing (flow) ) flow = .st0 (theme, "flow", TRUE)
	if (missing (lum) ) lum = .st0 (theme, "lum", TRUE)

	bso = list ()
	bso$nhl = nhl
	bso$ref.arrows = ref.arrows
	bso$rendering.style = rendering.style
	bso$top.color = top.color
	bso$side.color = side.color
	bso$sgrid.color = sgrid.color
	bso$iso.colors = iso.colors
	bso$main = main
	bso$hcv = hcv
	bso$glass = glass
	bso$flow = flow
	bso$lum = lum
	bso$theme = theme
	bso$test.mode = test.mode

	bso$barface = "barface"
	bso$litmus.fit = main

	options (barsurf=bso)
}

set.bs.nhl = function (nhl=1)
{	bso = getOption ("barsurf")
	bso$nhl = nhl
	options (barsurf=bso)
}

set.bs.theme = function (theme="blue")
{	bso = getOption ("barsurf")
	set.bs.options (nhl=bso$nhl, ref.arrows=bso$ref.arrows, rendering.style=bso$rendering.style, test.mode=bso$test.mode,
		theme=theme)
}

opt.nhl = function () getOption ("barsurf")$nhl

.fine.line.width = function ()
{	rs = getOption ("barsurf")$rendering.style
	if (rs == "pdf") 0.125
	else 1
}

opt.ref.arrows = function () getOption ("barsurf")$ref.arrows

st.top.color = function (theme) .sb0 ("top.color", theme)
st.side.color = function (theme) .sb0 ("side.color", theme)
st.sgrid.color = function (theme) .sgrid0 (theme)
st.iso.colors = function (theme) .sb0 ("iso.colors", theme)

st.litmus.fit = function (theme) .st0 (theme)
st.litmus.fit.hcv = function (theme) .st0 (theme, "hcv")
st.litmus.fit.flow = function (theme) .st0 (theme, "flow")
st.litmus.fit.lum = function (theme) .st0 (theme, "lum")

.st = function (name, eval.str=FALSE)
{	str = getOption ("barsurf") [[name]]
	if (eval.str)
		eval (str2lang (str) )
	else
		str
}

opt.top.color = function () .st ("top.color")
opt.side.color = function () .st ("side.color")
opt.sgrid.color = function () .st ("sgrid.color")
opt.iso.colors = function () .st ("iso.colors")

opt.litmus.fit = function () .st ("main", TRUE)
opt.litmus.fit.hcv = function () .st ("hcv", TRUE)
opt.litmus.fit.glass = function () .st ("glass", TRUE)
opt.litmus.fit.flow = function () .st ("flow", TRUE)
opt.litmus.fit.lum = function () .st ("lum", TRUE)
