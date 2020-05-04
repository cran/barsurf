\name{03_plot_contour_3d}
\alias{plot_contour_3d}
\alias{plotf_contour_3d}
\alias{nested_isosurfaces}
\title{Contour Plots in 3D}
\description{Contour plots in 3d (for functions of three variables), with one or more isosurfaces.}
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

Optionally, 2D contour lines may added to the base panel and rear panels.

If necessary, a vector of panel constants (pconstants) is used.

A matrix is computed by evaluating the function, f, while holding the third variable constant (at the third panel constant). This matrix is used to compute the base contours. Likewise, the first variable is held constant (at the first panel constant) for the right rear panel, and the second variable is held constant (at the second panel constant) for the left rear panel.

The nested_isosurfaces function calls the plotf_contour_3d function, but computes the grid size for each isosurface.

Unlike the other two functions, it needs at least two isosurfaces, and assumes that the first isosurface(s) is/are inside the second, and that the second isosurface(s) is/are inside the third, and so on.

By default, the resolution of fv is highest for the first isosurface, and each fv array has a progressively lower resolution.

Resulting plots should result in smaller file sizes, and render more quickly.
}
\usage{
#x-y-fv version
#(main argument is an array, or a list of arrays)
plot_contour_3d (x, y, z, fv, fb, \dots,
    wire.frame=FALSE,
    main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
    xlim, ylim, zlim,
    axes=TRUE, arrows=TRUE,
    ncontours=2, wire.frame.color="#808080", iso.colors)

#functional version
#(which calls the x-y-fv version above)
plotf_contour_3d (f, xlim, ylim=xlim, zlim=xlim, \dots,
    n=20, maximal=FALSE,
    base.contours=FALSE, rear.contours=FALSE,
    pconstants)

#functional version
#(which calls plotf_contour_3d above)
nested_isosurfaces (f, xlim, ylim=xlim, zlim=xlim, \dots,
    nfirst=30, nlast=15)
}
\arguments{
\item{x, y, z}{Sorted numeric vectors of x, y and z coordinates, or lists of such vectors.\cr If lists, then their lengths need to equal the number of isosurfaces.\cr (If fv is an array, then x, y and z are optional).}
\item{fv}{A 3-dimensional numeric array, or a list of such arrays.\cr If fv is a list, then its length needs to equal the number of isosurfaces.}
\item{fb}{Numeric vector of function values (or "levels"), for contour/isosurface values.\cr This is optional, however in general, you need to specify this for good plots.\cr\cr Note that if isosurfaces are nested, the first isosurface (for the first fb value) is assumed to be inside the second isosurface (for the second fb value), the second inside the third, and so on.\cr In general, if you need to plot functions with minimal (low-valued) focal points, fb values should be increasing, and if need to plot functions with maximal (high-valued) focal points, fb values should be decreasing.}
\item{wire.frame}{Logical, include the wire.frame lines.}
\item{main, xlab, ylab}{Strings, main/axes titles.}
\item{xat, yat}{Optional numeric vectors, the x and y axes tick points.}
\item{xlabs, ylabs}{Optional character vectors, the x and y axes tick labels.\cr In 3D plots, ignored unless axes is true and arrows is false.}
\item{xlim, ylim}{Length-2 numeric vectors, the x and y ranges.\cr Note that the package has only been tested with ascending values.}
\item{zlim}{Length-2 numeric vector, the z (vertical) range.}
\item{axes}{Logical vector of length one or two, if true plot reference arrows or axis ticks with labels.}
\item{arrows}{Logical vector of length one or two, (subject to axes, above) if true, plot reference arrows, if false, use axis ticks with labels.}
\item{ncontours}{Integer, the number of contours/isosurfaces, ignored if fb supplied.}
\item{wire.frame.color}{Optional string, giving the wire frame color.}
\item{iso.colors}{Character vector (with the same length as the number of isosurfaces), giving the (initial) isosurface colors. Optional for up to three isosurfaces.}
\item{f}{A numeric-valued function of three variables (x, y, and z).}
\item{n}{An integer vector of length one or three (giving the number of grid points), or list of such vectors (giving the number of grid points for each isosurface).\cr If a list, its length needs to equal the number of isosurfaces.}
\item{nfirst, nlast}{Integer vectors of length one or three (giving the number of grid points), for the first and last isosurfaces.}
\item{maximal}{Logical, if true assume maximal (high-valued) focal points, that is, any focal points within the plot have higher function/array values than their surrounding values.\cr This determines whether the default fb values are ascending or descending.\cr And is ignored if the fb vector is supplied.}
\item{base.contours}{Logical, include contour lines on the base panel of the plot.}
\item{rear.contours}{Logical, include contour lines on the rear panels of the plot.}
\item{pconstants}{Optional length-3 numeric vector of panel constants.\cr Defaults to the midpoints of xlim, ylim and zlim.\cr Refer to details and examples.}
\item{\dots}{In the x-y-fv version, ignored.}
}
\examples{
library (misc3d)

plotf_contour_3d (bispherical.dist, c (-3, 3),, c (-2, 2), fb = c (0.5, 1, 1.75),
    base.contours=TRUE)

#panel contours condition on x=1, y=1, z=0
nested_isosurfaces (bispherical.dist, c (-3, 3),, c (-2, 2), fb = c (0.5, 1, 1.75),
    base.contours=TRUE, rear.contours=TRUE,
    arrows=FALSE,
    pconstants = c (1, 1, 0) )
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
Plots of scalar fields over two variables:\cr
\link{plot_dfield}, \link{plot_cfield}

Other plots of scalar fields over three variables:\cr
\link{plot_cfield_3d}

Other functions:\cr
\link{litmus}, \link{litmus.fit} }
