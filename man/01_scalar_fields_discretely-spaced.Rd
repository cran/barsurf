\name{01_scalar_fields_discretely-spaced}
\alias{plot_dfield}
\alias{plot_bar}
\alias{plot_matrix}
\alias{plotf_dfield}
\alias{plotf_bar}
\title{Discretely-Spaced Scalar Fields}
\description{Plots of functions of two variables (which can be mapped to discretely-spaced scalar fields), and matrix visualization (per se).}
\details{
These functions produce combined contour-heat plots and 3d bar plots, for discretely-spaced scalar fields.

This can be used to plot numeric matrices (here, labelled as fv), and functions that can be mapped to numeric matrices.

In these plots, each heatmap bin represents one element from fv.

With the exception of plot_matrix, increasing rows in fv correspond to increasing x values and increasing columns in fv correspond to increasing y values. If an x vector is supplied its length needs to equal the number of rows (exact-match case) or the number of rows plus one (plus-one case). Likewise, if a y vector is supplied its length needs to equal the number of columns or the number of columns plus one. In the exact-match cases, the vectors give the midpoints of bins, and in the plus-one cases, vectors give the breakpoints of bins, including the outermost coordinates.

The plot_matrix function calls the plot_dfield function with fv transposed, such that increasing rows in fv correspond to increasing y values and increasing columns in fv correspond to increasing x values. Note that by default, plot_matrix also reverses the y axis, such that increasing y values (increasing rows) go from top to bottom. This means that by default, the resulting plots have the same orientation as standard matrices, in text form.

Currently, bar plots use a diamond-based projection with a fixed viewing angle, such that the origin is at the bottom center.

With the exception of plot_bar, if none of color.function, color.fit and cols are supplied, then:\cr
(1) A color.fit function is determined by global options.\cr
(2) The color.fit function is used to compute a color.function from fv.\cr
(3) The color.function is used to compute a color matrix from fv.

The color function can be any function that maps a numeric vector/matrix to a character vector/matrix of R colors, however, I recommend using litmus objects. The color fit function can be any function that maps a numeric vector to a valid color function, however, I recommend using a litmus.fit wrapper.

In plot_bar, the color function is a function that maps a logical (true or false) to an R color.

If there are missing values, the bars are omitted.\cr
(So, the plot_bar color function is never called with missing values).

Note that:\cr
(1) In the plot_* functions (not the plotf_* functions) the only argument that's required is fv.\cr
(2) There's no guarantee that default contour lines will be suitable.\cr
(3) Only opaque color functions should be used in heatmaps, that is, color functions that produce opaque colors with no transparency. However, bar plots may use semitransparent colors.
}
\usage{
plot_dfield (x, y, fv, fb, \dots,
    grid.lines, contours=TRUE,
    heatmap=TRUE, contour.labels=FALSE,
    main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
    xyrel = test.xyrel (x, y, fv), as.matrix=FALSE,
    add=FALSE, axes=TRUE, reverse = c (FALSE, as.matrix),
    ncontours=2, clabs,
    grid.col, contour.col="#000000",
    color.function, color.fit, cols, hcv=FALSE)

plot_bar (x, y, fv, \dots,
    main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
    zlim,
    axes=TRUE, arrows=TRUE,
    color.function, cols)

plot_matrix (x, y, fv, fb, \dots,
    grid.lines, contours=FALSE,
    heatmap=TRUE, contour.labels=FALSE,
    main, xlab="col", ylab="row", xat, yat, xlabs, ylabs,
    xyrel = test.xyrel (x, y, fv), as.matrix=TRUE,
    add=FALSE, axes=TRUE, reverse = c (FALSE, as.matrix),
    ncontours=2, clabs,
    grid.col, contour.col="#000000",
    color.function, color.fit, cols, hcv=TRUE)

#functional versions
plotf_dfield (f, xlim, ylim=xlim, \dots)
plotf_bar (f, xlim, ylim=xlim, zlim, \dots)
}
\arguments{
\item{x, y}{Optional sorted vectors of x and y midpoints or breakpoints, refer to details.}
\item{fv}{A numeric matrix (representing a discretely-spaced scalar field), which may include NAs.}
\item{fb}{Optional vector of function values (or "levels"), for contours.}
\item{grid.lines}{Logical, include grid lines. Defaults to true, if is.matrix is true and both the number of rows and columns don't exceed 20.}
\item{contours}{Logical, include contour lines.}
\item{heatmap}{Logical, include heatmap.}
\item{contour.labels}{Logical, include contour labels.}
\item{main}{.}
\item{xlab, ylab}{.}
\item{xat, yat}{The x and y axes tick points.}
\item{xlabs, ylabs}{The x and y axes tick labels.\cr In 3D plots, ignored unless axes is true and arrows is false.}
\item{xyrel}{Single character, either "f", "s" or "m". "f" produces a plot with a fixed aspect ratio of one, "s" produces a square plot, and "m" a maximized plot.}
\item{as.matrix}{Logical, plot fv with the same orientation as a textual representation of a matrix. Note that the overall result depends on the value of reverse.}
\item{add}{Logical, if true add contours/heatmap to an existing plot.}
\item{axes}{Logical vector of length one or two, if true plot reference arrows or axis ticks with labels.}
\item{arrows}{Logical vector of length one or two, (subject to axes, above) if true, plot reference arrows, if false, use axis ticks with labels.}
\item{reverse}{Logical vector of one or two, to reverse the x and y axes.}
\item{ncontours}{Integer, the number of contour lines.\cr Ignored if fb supplied.}
\item{clabs}{Optional character vector of contour labels.}
\item{grid.col}{String, giving the grid line color.}
\item{contour.col}{Character vector of length one or with the same length as the number of contours, giving the contour line colors.}
\item{color.function}{Optional color function, such as a barface object (for plot_bar) or an opaque litmus object (for the other plots), refer to details.}
\item{color.fit}{Optional color fitting function, such as a litmus.fit wrapper, refer to details.}
\item{cols}{Optional character matrix of R colors, for the heatmap bins. For plot_bar, this may also be a list with two matrices, one for tops and one for sides.}
\item{hcv}{Logical, use the high color variation option.}
\item{f}{A numeric-valued function of two variables (x and y), suitable for use with base::outer.}
\item{xlim, ylim}{The x and y ranges.}
\item{zlim}{The vertical range, corresponding to the value of the function or matrix.}
\item{\dots}{Ignored.}
}
\examples{
fv = matrix (sample (1:100), 10, 10)

plot_dfield (,,fv)
plot_bar (,,fv)
plot_matrix (,,fv)
}
\references{Refer to the vignette for an overview, references and better examples.}
