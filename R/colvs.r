#barsurf: Multivariate Function Visualization and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.colv = function (color.space, H, C, L)
	list (colvs = cbind (H, C, L), color.space=color.space)
.colm = function (color.space, ..., nc=3)
	list (colvs = matrix (unlist (list (...) ),, nc, byrow=TRUE), color.space=color.space)

.sgold = list (main="blue", hcv = c ("blue", "hcv"), flow = c ("blue", "flow"), lum = c ("gold", "lum") )
.sblue = list (main="blue", hcv = c ("blue", "hcv"), flow = c ("blue", "flow"), lum = c ("blue", "lum") )
.sgreen = list (main="green", hcv = c ("green", "hcv"), flow = c ("green", "flow"), lum = c ("green", "lum") )
.spurple = list (main="blue", hcv = c ("blue", "hcv"), flow = c ("blue", "flow"), lum = c ("purple", "lum") )
.sheat = list (main="heat", hcv="heat", flow="heat", lum = c ("heat", "lum") )

.gold.bar = c (
	77, 77, 70, 0.95,
	77, 70, 77, 0.85)
.blue.bar = c (
	220, 30, 67.5, 0.95,
	220, 30, 75, 0.85)
.green.bar = c (
	137.5, 60, 70, 0.95,
	137.5, 55, 75, 0.85)
.purple.bar = c (
	285, 17.5, 65, 0.95,
	285, 17.5, 72.5, 0.85)
.heat.bar = c (
	20, 65, 60, 0.95,
	20, 55, 67.5, 0.85)

.sgrid.colors = c (gold="#D0D000", blue="#707088", green="#608060", purple="#685068", heat="#804040")

.gold.iso = c ("#B0A030B0", "#80404020", "#40800008")
.blue.iso = c ("#00208020", "#00408010", "#00208008")
.green.iso = c ("#00802020", "#00804010", "#00802008")
.purple.iso = c ("#700098B0", "#80008030", "#80008008")
.heat.iso = c ("#FF1800B0", "#FF300030", "#FF180008")

.gold.iso = c (
	70, 40, 50, 0.9,
	75, 40, 60, 0.25,
	80, 40, 70, 0.1)
.blue.iso = c (
	240, 40, 45, 0.9,
	245, 60, 45, 0.25,
	240, 40, 45, 0.05)
.green.iso = c (
	120, 20, 45, 0.9,
	125, 30, 45, 0.25,
	120, 20, 45, 0.05)
.purple.iso = c (
	275, 40, 40, 0.9,
	270, 60, 40, 0.25,
	275, 40, 40, 0.05)
.heat.iso = c (
	20, 40, 40, 0.9,
	25, 60, 40, 0.25,
	20, 40, 40, 0.05)

.blue = .colv ("HCL", c (180, 210, 220, 275), 35, 75)
.green = .colv ("HCL", c (170, 140, 130, 95), 35, 75)
.heat = .colm ("sRGB",
	1, 0, 0,
	1, 0.40, 0.0,
	1, 0.65, 0.0,
	1, 0.9, 0.275,
	1, 1, 1)

.blue.hcv = .colm ("HCL",
	220, 50, 45,
	220, 50, 55,
	220, 50, 65,
	210, 50, 75,
	210, 12.5, 85,
	210, 0, 90)
.green.hcv = .colm ("HCL",
	140, 65, 50,
	130, 65, 60,
	120, 65, 70,
	110, 65, 80,
	100, 12.5, 90,
	90, 0, 95)

.blue.flow = .colm ("HCL",
	220, 40, 55,
	0, 0, 90)
.green.flow = .colm ("HCL",
	140, 40, 55,
	0, 0, 90)

.gold.lum	= .colm ("HCL", nc=4,
	77, 65, 65, 0.875,
	77, 0, 85, 0.875)
.blue.lum = .colm ("HCL", nc=4,
	220, 30, 57.5, 0.875,
	0, 0, 85, 0.875)
.green.lum = .colm ("HCL", nc=4,
	137.5, 30, 57.5, 0.875,
	0, 0, 85, 0.875)
.purple.lum	= .colm ("HCL", nc=4,
	285, 30, 57.5, 0.875,
	0, 0, 85, 0.875)
.heat.lum = .colm ("HCL", nc=4,
	15, 80, 50, 0.875,
	0, 0, 85, 0.875)

.hc.chroma = c (35, 50, 50, 50, 65)
.hc.lum = c (60, 65, 65, 65, 60)
