\name{05_vector_fields}
\alias{plot_vecfield}
\alias{plotf_vecfield}
\title{Vector Fields}
\description{Plots of vector fields.}
\details{
Refer to plot_cfield for background information.

The plot_vecfield function is the same, except that:\cr
(1) There are vector arrows rather than contour lines.\cr
(2) fv is replaced by two matrices, for the x and y components.\cr
(3) The color of the heatmap is determined by the magnitude of the vectors, similar to plot_surface.
(4) Missing values are allowed.

The plotf_vecfield function calls plot_vecfield twice, and by default, uses a higher resolution for the heatmap than the arrows.

If you want to plot a vector field with a subset of vector arrows then I recommend you plot the vector field with vectors=FALSE, then plot another vector field on top of it with add=TRUE, heatmap=FALSE, and set parts of fx or fy to NA. A similar approach can be used to plot vector fields with arrows that have different sizes or colors.

Note that:\cr
(1) In the plot_vecfield the only arguments that are required are fx and fy.\cr
(2) In publication graphics, you may want to increase the resolution of the heatmap.\cr
(3) Only opaque color functions should be used, that is, color functions that produce opaque colors with no transparency.
}
\usage{
#x-y-fx-fy version
#(main arguments are matrices)
plot_vecfield (x, y, fx, fy, \dots,
    vectors=TRUE, heatmap=TRUE, all=FALSE,
    main, xlab="x", ylab="y", xat, yat, xlabs, ylabs,
    xyrel = test.xyrel (x, y, fv),
    add=FALSE, axes=TRUE, reverse=FALSE,
    arrowh.length=1.75, arrowh.width = 0.75 * arrowh.length,
    arrow.color="#000000", fill.color="#08080810",
    color.function, color.fit, hcv=FALSE)

#functional version
#(which calls the x-y-fx-fy version above)
plotf_vecfield (f, xlim, ylim=xlim, \dots, nv=20, nh=40)
}
\arguments{
\item{x, y}{Optional sorted numeric vectors of x and y coordinates, refer to details.}
\item{fx, fy}{Numeric matrices (representing x and y components of a vector field), which may include NAs.}
\item{vectors}{Logical, include vector arrows.}
\item{heatmap}{Logical, include heatmap.}
\item{all}{Logical, plot all vector arrows. Otherwise (the default), exclude the outermost rows and columns.}
\item{main, xlab, ylab}{Optional strings, main/axes titles.}
\item{xat, yat}{Optional numeric vectors, the x and y axes tick points.}
\item{xlabs, ylabs}{Optional character vectors, the x and y axes tick labels.}
\item{xyrel}{Single character, either "f", "s" or "m". "f" produces a plot with a fixed aspect ratio of one, "s" produces a square plot, and "m" a maximized plot.}
\item{add}{Logical, if true add contours/heatmap to an existing plot.}
\item{axes}{Logical vector of length one or two, if true plot axis ticks with labels.}
\item{reverse}{Logical vector of one or two, to reverse the x and y axes.\cr Note that this argument could change.}
\item{arrowh.length}{Arrow head length, in mm.}
\item{arrowh.width}{Arrow head width, in mm.}
\item{arrow.color}{Arrow line color.}
\item{fill.color}{Arrow fill color.}
\item{color.function}{Optional color function, such as an opaque litmus object, refer to details.}
\item{color.fit}{Optional color fitting function, such as a litmus.fit wrapper, refer to details.}
\item{hcv}{Logical, use the high color variation option, refer to details.}
\item{f}{A function of two variables (x and y), which returns a list of two numeric vectors, one value for the x component and one for the y component.}
\item{xlim, ylim}{Length-two numeric vectors, the x and y ranges.\cr Currently, these need to be ascending.}
\item{nv, nh}{Integer vectors of length one or two, giving the number of grid points, where nv is for the vector arrows and nh is for the heatmap.}
\item{\dots}{In the x-y-fx-fy version, ignored.}
}
\examples{
plotf_vecfield (concentric.field, c (-1.5, 1.5), c (-1.5, 1.5) )
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{\link{plot_cfield}

\link{litmus}, \link{litmus.fit}

\link{test.xyrel} }

