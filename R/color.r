rgb2hcl = function (colv)
	coords (as (RGB (colv [1], colv [2], colv [3]), "polarLUV") )[3:1]

hcl2rgb = function (colv, correction=TRUE)
{	colv = coords (as (polarLUV (colv [3], colv [2], colv [1]), "sRGB") )
	if (correction)
	{	colv [colv < 0] = 0
		colv [colv > 1] = 1
	}
	colv
}

use.theme = function (theme)
{	bso = list ()
	if (theme == "blue")
	{	bso$plot2d.cell.colv.1 = c (120, 30, 80)
		bso$plot2d.cell.colv.2 = c (300, 30, 80)
		bso$plot2d.cell.colv.na = c (0, 0, 90)
		bso$plot2d.contour.colv.1 = c (120, 30, 80)
		bso$plot2d.contour.colv.2 = c (300, 30, 80)
		bso$plot3d.bar.colv.1 = c (220, 30, 67.5)
		bso$plot3d.bar.colv.2 = c (220, 30, 77.5)
		bso$plot3d.surface.colv.1 = c (220, 30, 62.5)
		bso$plot3d.surface.colv.2 = c (0, 0, 87.5)
	}
	else if (theme == "green")
	{	bso$plot2d.cell.colv.1 = c (160, 60, 87.5)
		bso$plot2d.cell.colv.2 = c (85, 60, 87.5)
		bso$plot2d.cell.colv.na = c (0, 0, 90)
		bso$plot2d.contour.colv.1 = c (160, 60, 87.5)
		bso$plot2d.contour.colv.2 = c (85, 60, 87.5)
		bso$plot3d.bar.colv.1 = c (137.5, 60, 70)
		bso$plot3d.bar.colv.2 = c (137.5, 60, 80)
		bso$plot3d.surface.colv.1 = c (137.5, 60, 62.5)
		bso$plot3d.surface.colv.2 = c (0, 0, 87.5)
	}
	else
		stop ("unsupported theme")
	options (barsurf=bso)
}

test.theme = function ()
{	x = y = 1:4
	f = function (x, y) x + y
	z1 = outer (x, y, f)

	x = y = 1:12
	f = function (x, y) x ^ 2 + y ^ 2
	z2 =outer (x, y, f)

	p0 = par (mfrow=c (2, 2), mar=c (3, 3, 1, 1) )
	plot2d.cell (,,z1, xlab="", ylab="")
	plot2d.contour (,,z2, xlab="", ylab="")
	plot3d.bar (,,z1)
	plot3d.surface (,,z2)
	par (p0)
}

.interpolate.rgb = function (colv.1, colv.2, w)
{	colv = colv.1 + (colv.2 - colv.1) * w
	rgb (colv [1], colv [2], colv [3])
}

.interpolate.hcl = function (colv.1, colv.2, w)
{	if (colv.1 [2] == colv.2 [2] && colv.1 [3] == colv.2 [3])
		.interpolate.hue (colv.1, colv.2, w)
	else
		.interpolate.rgb (hcl2rgb (colv.1), hcl2rgb (colv.2), w)
}

.interpolate.hue = function (colv.1, colv.2, w)
{	h = colv.1 [1] + (colv.2 [1] - colv.1 [1]) * w
	hcl (h, colv.1 [2], colv.1 [3])
}
