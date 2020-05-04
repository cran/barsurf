\name{04_plot_cfield_3d}
\alias{plot_cfield_3d}
\alias{plotf_cfield_3d}
\title{Contour-Heat Plots, 3D-Based}
\description{Plots of continuously-spaced scalar fields, of three variables.}
\details{
These functions are similar to plot_cfield, plot_contour_3d, plot_surface and plotf_contour_3d.\cr
(So, please refer to those functions for background information).

These functions produce 3d-based combined contour-heat plots.

Plots contain a set of of two or more 2d slides (or slices), which can be used to plot functions of three variables, x, y and z.

In plot_cfield_3d, fv is a list of matrices, rather than a single matrix.

Each matrix represents one slide.

In general, six or seven slides produces a good result. More slides may be used, but with care.

x and y (the first two variables) have the same interpretation as they do in plot_surface. z (the third variable) gives the vertical position of the slides.

Note that:\cr
(1) In plot_cfield_3d the only argument that is required is fv.\cr
(2) In some cases, the plotting device/window should be opened before calling these functions, and the plot should be higher than it is wide.\cr
(3) There's no guarantee that default contour lines will be suitable.\cr
(4) Unlike other heatmaps in this package, these functions may use semitransparent colors.

Expanding on point (4), some PDF viewers may produce visual artifacts, such as grid lines.

In general, these artifacts are minimal.

If you want to remove them, possible options are to change your PDF viewer's settings or save the plot in a raster format, such as PNG.
}
\usage{
#x-y-fv version
#(main argument is a list of matrices)
plot_cfield_3d (x, y, z, fv, fb, \dots,
    contours=TRUE, heatmap=TRUE,
    main, xlab="x", ylab="y",
    axes=TRUE, reverse.z=FALSE,
    ncontours=6, emph="n", color.function, color.fit)

#functional version
#(which calls the x-y-fv version, above)
plotf_cfield_3d (f, xlim, ylim=xlim, zlim=xlim, \dots, nslides=6, n=30, z)
}
\arguments{
\item{x, y}{Optional sorted numeric vectors of x and y coordinates, refer to details.}
\item{z}{Optional numeric vector of two or more z coordinates, refer to details.}
\item{fv}{A list of two or more numeric matrices (representing a continuously-spaced scalar fields), one for each z value.}
\item{fb}{Optional numeric vector of function values (or "levels"), for contours.}
\item{contours}{Logical, include contour lines.}
\item{heatmap}{Logical, include heatmap.}
\item{main, xlab, ylab}{Strings, main/axes titles.}
\item{axes}{Logical vector of length one or two, plot reference arrows.}
\item{reverse.z}{Logical, if true reverse the z axis.}
\item{ncontours}{Integer, the number of contour lines.\cr Ignored if fb supplied.}
\item{emph}{What to emphasize, either "n" (for nothing), "b" for both (low and high regions), "l" for low regions or "h" for high regions, or "B", "L" or "H" for the same but with a stronger effect.}
\item{color.function}{Optional color function, such as a litmus object, refer to details.}
\item{color.fit}{Optional color fitting function, such as a litmus.fit wrapper, refer to details.}
\item{f}{A numeric-valued function of three variables (x, y, and z), suitable for use with base::outer, called with four arguments, x, y, f, and z [i].}
\item{xlim, ylim}{Length-2 numeric vectors, the x and y ranges.\cr Currently, these need to be ascending.}
\item{zlim}{Length-2 numeric vector, the z (vertical) range, ignored is z supplied.}
\item{nslides}{Integer, giving the number of slides, ignored if z supplied.}
\item{n}{An integer vector of length one or two, giving the number of grid points.}
\item{\dots}{In the x-y-fv version, ignored.}
}
\examples{
plotf_cfield_3d (bispherical.dist, c (-3, 3),, c (-2, 2), emph="l")
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
Plots of scalar fields over two variables:\cr
\link{plot_dfield}, \link{plot_cfield}

Other plots of scalar fields over three variables:\cr
\link{plot_contour_3d}

Other functions:\cr
\link{litmus}, \link{litmus.fit} }
