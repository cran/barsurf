\name{02_scalar_fields_continuously-spaced}
\alias{plot_cfield}
\alias{plot_surface}
\alias{plot_tricontour}
\alias{plot_trisurface}
\alias{plotf_cfield}
\alias{plotf_surface}
\alias{plotf_tricontour}
\alias{plotf_trisurface}
\title{Continuously-Spaced Scalar Fields}
\description{Plots of functions of two variables (which can be mapped to continuously-spaced scalar fields), for both rectangular and triangular areas.}
\details{
These functions produce combined contour-heat plots and 3d surface plots, for rectangular (and triangular) continuously-spaced scalar fields.

This can be used to plot numeric matrices (here, labelled as fv), and functions that can be mapped to numeric matrices.

In contrast to the discretely-spaced case, each heatmap bin represents one area between four (or three) fv points.

Increasing rows in fv correspond to increasing x values and increasing columns in fv correspond to increasing y values. If the x vector is supplied its length needs to equal the number of rows. Likewise, if the y vector is supplied its length needs to equal the number of columns.

Currently, surface plots use a diamond-based projection with a fixed viewing angle, such that the origin is at the bottom center.

If none of color.function and color.fit are supplied, then:\cr
(1) A color.fit function is determined by global options.\cr
(2) The color.fit function is used to compute a color.function from interpolated bin values (in heatmaps) or the magnitudes of their gradients (for surface plots).\cr
(3) The color.function is used to compute a color matrix from the interpolated bin values or magnitudes.

The color function can be any function that maps a numeric vector/matrix to a character vector/matrix of R colors, however, I recommend using litmus objects. The color fit function can be any function that maps a numeric vector to a valid color function, however, I recommend using a litmus.fit wrapper.

Note that:\cr
(1) In the plot_* functions (not the plotf_* functions) the only argument that's required is fv.\cr
(2) There's no guarantee that default contour lines will be suitable.\cr
(3) If using plot_surface or plot_trisurface to plot near-constant values, set zlim, for a better result.\cr
(4) In publication graphics, you may want to increase the resolution of the heatmap.\cr
(5) Only opaque color functions should be used in heatmaps, that is, color functions that produce opaque colors with no transparency. However, surface plots may use semitransparent colors.

Expanding on point (3), functions that are constant in theory, may product non-constant (but near-constant) values when computed via floating point arithmetic.
}
\usage{
#x-y-fv versions
#(main argument is a matrix)
plot_cfield (x, y, fv, fb, \dots,
    contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
    main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
    xyrel = test.xyrel (x, y, fv),
    add=FALSE, axes=TRUE, reverse=FALSE,
    ncontours=6, clabs,
    contour.color="#000000",
    color.function, color.fit, hcv=FALSE)

plot_surface (x, y, fv, \dots,
    grid.lines=TRUE,
    main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
    zlim, axes=TRUE, arrows=TRUE,
    grid.color, color.function, color.fit)

plot_tricontour (x, y, fv, fb, \dots,
    contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
    main, xlab="x", ylab="y",
    xyrel="s",
    axes=TRUE,
    ncontours=6, clabs,
    contour.color="#000000",
    color.function, color.fit, hcv=FALSE)

plot_trisurface (x, y, fv, \dots,
    grid.lines=TRUE,
    main, xlab="x", ylab="y",
    zlim, axes=TRUE, arrows=TRUE,
    grid.color, color.function, color.fit)

#functional versions
#(which call the x-y-fv versions above)
plotf_cfield (f, xlim, ylim=xlim, \dots, n=30)
plotf_surface (f, xlim, ylim=xlim, zlim, \dots, n=30)
plotf_tricontour (f, \dots, n=30)
plotf_trisurface (f, \dots, n=30)
}
\arguments{
\item{x, y}{Optional sorted numeric vectors of x and y coordinates, refer to details.\cr In the triangular case, ignored.}
\item{fv}{A numeric matrix (representing a continuously-spaced scalar field), which may not include NAs.\cr In the triangular case, fv needs to be a square matrix, but only the top-left triangle is used, including the diagonal.}
\item{fb}{Optional numeric vector of function values (or "levels"), for contours.}
\item{grid.lines}{Logical, include grid lines.}
\item{contours}{Logical, include contour lines.}
\item{heatmap}{Logical, include heatmap.}
\item{contour.labels}{Logical, include contour labels.}
\item{main, xlab, ylab}{Optional strings, main/axes titles.}
\item{xat, yat}{Optional numeric vectors, the x and y axes tick points.}
\item{xlabs, ylabs}{Optional character vectors, the x and y axes tick labels.\cr In 3D plots, ignored unless axes is true and arrows is false.}
\item{xyrel}{Single character, either "f", "s" or "m". "f" produces a plot with a fixed aspect ratio of one, "s" produces a square plot, and "m" a maximized plot.}
\item{add}{Logical, if true add contours/heatmap to an existing plot.}
\item{axes}{Logical vector of length one or two, if true plot reference arrows or axis ticks with labels.}
\item{arrows}{Logical vector of length one or two, (subject to axes, above) if true, plot reference arrows, if false, use axis ticks with labels.}
\item{reverse}{Logical vector of one or two, to reverse the x and y axes.\cr Note that this argument could change.}
\item{ncontours}{Integer, the number of contour lines.\cr Ignored if fb supplied.}
\item{clabs}{Optional character vector of contour labels.}
\item{grid.color}{String, giving the grid line color.}
\item{contour.color}{Character vector of length one or with the same length as the number of contours, giving the contour line colors.}
\item{color.function}{Optional color function, such as an arbitrary litmus object (for plot_surface) or an opaque litmus object (for the other plots), refer to details.}
\item{color.fit}{Optional color fitting function, such as a litmus.fit wrapper, refer to details.}
\item{hcv}{Logical, use the high color variation option, refer to details.}
\item{f}{In the rectangular case, a numeric-valued function of two variables (x and y), suitable for use with base::outer.\cr In the triangular case, a similar function, but of three variables (w1, w2, w3) or (x, y and z) etc, with values between zero and one, which in general, sum to one.}
\item{xlim, ylim}{Length-2 numeric vectors, the x and y ranges.\cr Currently, these need to be ascending.}
\item{zlim}{Length-2 numeric vector, the vertical range, corresponding to the value of the function or matrix.}
\item{n}{An integer vector of length one or two, giving the number of grid points.}
\item{\dots}{In the x-y-fv versions, ignored.}
}
\examples{
x <- y <- seq (-15.5, 15.5, length.out=40)
fv <- outer (x, y, rotated.sinc)

plot_cfield (,,fv)
plot_surface (,,fv)
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
Discretely-spaced versions:\cr
\link{plot_dfield}

Plots of scalar fields over three variables:\cr
\link{plot_contour_3d}, \link{plot_cfield_3d}

Other functions:\cr
\link{litmus}, \link{litmus.fit}

\link{test.xyrel} }
