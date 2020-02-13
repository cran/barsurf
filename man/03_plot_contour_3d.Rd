\name{03_plot_contour_3d}
\alias{plot_contour_3d}
\alias{plotf_contour_3d}
\alias{nested_isosurfaces}
\title{Contour Plots in 3D}
\description{Contour plots in 3d, with one or more isosurfaces.}
\details{
These functions require the misc3d package to be installed, and be on the search path.

These functions are similar to plot_cfield and plot_surface.\cr
(So, please refer to those functions for background information).

In plot_contours_3d, fv is a 3-dimensional array, or a list of such arrays.

x, y and z can be vectors, or lists of vectors. 

Increasing along the first array dimension corresponds to increasing x values. Increasing along the second array dimension corresponds to increasing y values. And increasing along the third array dimension corresponds to increasing z values.

x and y (the first two variables) have the same interpretation as they do in plot_surface. z (the third variable) gives the vertical position. Except that x, y and z describe coordinates of the fv array, not the resulting isosurfaces.

If x, y, z, fv or n are lists, then their lengths need to equal the number of isosurfaces, and each list element applies to one isosurface.

Unlike other plots in this package, the isosurfaces don't use color interpolation, however, a small amount of random color variation is added.

The plotf_contour_3d function calls plot_contour_3d, but computes the x, y, z and fv values.

By default, 2D contour lines are added to the base.

The nested_isosurfaces function calls the plotf_contour_3d function, but computes the grid size.

Unlike the other two functions, it needs at least two isosurfaces, and assumes that the first isosurface(s) is/are inside the second, and that the second isosurface(s) is/are inside the third, and so on.

By default, the resolution of fv is highest for the first isosurface, and each fv array has a progressively lower resolution.

Resulting plots should result in smaller file sizes, and render more quickly.
}
\usage{
#array version
plot_contour_3d (x, y, z, fv, fb, \dots,
    wire.frame=FALSE,
    main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
    xlim, ylim, zlim,
    axes=TRUE, arrows=TRUE,
    ncontours=2, wire.frame.col="#808080", iso.cols)

#functional versions
plotf_contour_3d (f, xlim, ylim=xlim, zlim=xlim, fb, \dots,
    base.contours=TRUE, n=20)
nested_isosurfaces (f, xlim, ylim=xlim, zlim=xlim, fb, \dots,
    base.contours=TRUE, nfirst=30, nlast=15)
}
\arguments{
\item{x, y, z}{Sorted vectors of x, y and z coordinates, or lists of such vectors.\cr If lists, then their lengths need to equal the number of isosurfaces.\cr (If fv is an array, then x, y and z are optional).}
\item{fv}{A 3-dimensional array, or a list of such arrays.\cr If fv is a list, then its length needs to equal the number of isosurfaces.}
\item{fb}{Vector of function values (or "levels"), for contour/isosurface values.\cr This is optional, however in general, you need to specify this for good plots.}
\item{wire.frame}{Logical, include the wire.frame lines.}
\item{main}{.}
\item{xlab, ylab}{.}
\item{xat, yat}{The x and y axes tick points.}
\item{xlabs, ylabs}{The x and y axes tick labels.\cr In 3D plots, ignored unless axes is true and arrows is false.}
\item{xlim, ylim}{The x and y ranges.}
\item{zlim}{The z (vertical) range.}
\item{axes}{Logical vector of length one or two, if true plot reference arrows or axis ticks with labels.}
\item{arrows}{Logical vector of length one or two, (subject to axes, above) if true, plot reference arrows, if false, use axis ticks with labels.}
\item{ncontours}{Integer, the number of contours/isosurfaces, ignored if fb supplied.}
\item{wire.frame.col}{Optional string, giving the wire frame col.}
\item{iso.cols}{Character vector (with the same length as the number of isosurfaces), giving the (initial) isosurface colors. Optional for up to three isosurfaces.}
\item{f}{A numeric-valued function of three variables (x, y, and z).}
\item{\dots}{Ignored.}
\item{base.contours}{Logical, include contour lines on the base of the plot.}
\item{n}{An integer vector of length one or three (giving the number of grid points), or list of such vectors (giving the number of grid points for each isosurface).\cr If a list, its length needs to equal the number of isosurfaces.}
\item{nfirst, nlast}{Integer vectors of length one or three (giving the number of grid points), for the first and last isosurfaces.}
}
\examples{
library (misc3d)

plotf_contour_3d (bispherical.dist, c (-3, 3),, c (-2, 2), c (0.5, 1, 1.75) )
nested_isosurfaces (bispherical.dist, c (-3, 3),, c (-2, 2), c (0.5, 1, 1.75) )
}
\references{Refer to the vignette for an overview, references and better examples.}

