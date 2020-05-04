#barsurf: Heatmap-Related Plots and Smooth Multiband Color Interpolation
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.onLoad = function (...)
	set.bs.options ()

.EXTEND = function (object, class=NULL, ...)
{	class = c (class, class (object) )
	structure (object, class=class, ...)
}

.THAT = function () 
{	this = sys.function (-1)
	attributes (this)
}
