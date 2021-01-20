\name{11_main_plotting_functions}
\alias{Main Plotting Functions}
\alias{plot_dfield}
\alias{plot_cfield}
\alias{plot_bar}
\alias{plot_surface}
\alias{plot_tricontour}
\alias{plot_trisurface}
\alias{plot_isosurface}
\alias{plot_cfield3}
\alias{plot_matrix}
\alias{plot_vec}
\alias{plot_vec3}
\title{Main Plotting Functions}
\description{
Selected multivariate plots, including:\cr
Combined contour-heatmap plots, 3d bar and surface plots, triangular plots, isosurface plots, 3d-based contour-heatmap plots, and 2d/3d vector fields.

Note that there are wrappers for most of these plotting functions, which take a mathematical function as their main argument.

Refer to \link{Functional Versions}.
}
\usage{
####################################
#combined contour-heatmap plots
####################################
plot_dfield (x, y, fv, \dots, fb,
    main="", xlab="x", ylab="y",
    contours=TRUE, grid=FALSE, heatmap=TRUE,
    contour.labels=FALSE, bin.labels=FALSE,
    add=FALSE, axes=TRUE, reverse=FALSE, swap.sides=FALSE,
    xyrel, continuous.axes=FALSE,
    ncontours=2, clabs, blabs,
    xat, yat, xlabs, ylabs,
    raster=FALSE,
    contour.color="#000000", grid.color="#888888", 
    colf, colff, theme, colors, hcv=FALSE)

plot_cfield (x, y, fv, \dots, fb,
    main="", xlab="x", ylab="y",
    contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
    add=FALSE, axes=TRUE, reverse=FALSE, swap.sides=FALSE,
    ncontours=6, clabs,
    xyrel, xat, yat, xlabs, ylabs,
    raster=FALSE,
    contour.color="#000000", colf, colff, theme, hcv=FALSE)

####################################
#3d plots
#2 variables, f (x, y)
####################################
plot_bar (x, y, fv, \dots,
    main="", xlab="x", ylab="y", zlab="z",
    axes=TRUE, z.axis=FALSE,
    ref.arrows = opt.ref.arrows (), reverse=FALSE,
    continuous.axes=FALSE,
    zlim, xat, yat, xlabs, ylabs,
    nhl = opt.nhl (),
    top.color = st.top.color (theme), side.color = st.side.color (theme),
    theme)

plot_surface (x, y, fv, \dots,
    main="", xlab="x", ylab="y", zlab="z",
    grid=TRUE, gradient.shading=TRUE,
    axes=TRUE, z.axis=FALSE,
    ref.arrows = opt.ref.arrows (), reverse=FALSE,
    zlim, xat, yat, xlabs, ylabs,
    nhl = opt.nhl (),
    grid.color = st.sgrid.color (theme),
    colf, colff, theme)

####################################
#triangular plots
####################################
plot_tricontour (x, y, fv, \dots, fb,
    main="", xlab="x", ylab="y",
    contours=TRUE, heatmap=TRUE, contour.labels=FALSE,
    axes=TRUE, ncontours=6, clabs, xyrel="s",
    xat, yat, xlabs, ylabs,
    contour.color="#000000", colf, colff, theme, hcv=FALSE)

plot_trisurface (x, y, fv, \dots,
    main="", xlab="x", ylab="y", zlab="z",
    grid=TRUE,
    axes=TRUE, z.axis=FALSE,
    ref.arrows = opt.ref.arrows (),
    zlim,
    xat, yat, xlabs, ylabs,
    nhl = opt.nhl (),
    grid.color = st.sgrid.color (theme),
    colf, colff, theme)

####################################
#3d plots
#3 variables, f (x, y, z)
####################################
plot_isosurface (x, y, z, fv, \dots, fb, fq,
    main="", xlab="x", ylab="y", zlab="z",
    wire.frame=FALSE, texture=TRUE,
    axes=TRUE, z.axis = all (axes),
    ref.arrows = opt.ref.arrows (), z.ref.arrow = any (ref.arrows),
    reverse=FALSE, z.reverse=FALSE,
    nsurfaces=2,
    nested=TRUE, maximal,
    xat, yat, xlabs, ylabs,
    nhl = opt.nhl (),
    wire.color="#808080", iso.colors = st.iso.colors (theme),
    theme)

plot_cfield3 (x, y, z, fv, \dots, fb,
    main="", xlab="x", ylab="y", zlab="z",
    contours=TRUE, heatmap=TRUE,
    axes=TRUE, ref.arrows = opt.ref.arrows (),
    slide.labels = any (axes),
    reverse=FALSE, z.reverse=FALSE,
    ncontours=6, emph="n",
    xat, yat, xlabs, ylabs, zlabs,
    nhl = opt.nhl (),
    colf, colff, theme)

####################################
#matrix visualization
####################################
plot_matrix (x, y, fv, \dots, transpose=TRUE,
    xlab, ylab,
    contours=FALSE, grid, bin.labels,
    reverse = c (FALSE, transpose), swap.sides=reverse,
    blabs, colors,
    hcv=TRUE)

####################################
#vector fields
####################################
plot_vec (x, y, dx, dy, \dots,
    main="", xlab="x", ylab="y",
    vectors=TRUE, heatmap=TRUE,
    include.outermost.vectors=FALSE,
    add=FALSE, axes=TRUE, reverse=FALSE, swap.sides=FALSE,
    xyrel, xat, yat, xlabs, ylabs,
    arrowh.length=1.75, arrowh.width = 0.75 * arrowh.length,
    arrow.color="#000000", fill.color="#08080810",
    colf, colff, theme, hcv=FALSE)

plot_vec3 (x, y, z, dx, dy, dz, \dots, P = 1 / 3,
    main="", xlab="x", ylab="y", zlab="z",
    include.outermost.vectors=FALSE,
    axes=TRUE, z.axis = all (axes),
    ref.arrows = opt.ref.arrows (), z.ref.arrow = any (ref.arrows),
    xat, yat, xlabs, ylabs,
    nhl = opt.nhl (),
    head.color="#0000B0", mid.color="#8080FF", tail.color="#B0B0B0")
}
\arguments{
\item{x, y, z}{
    \bold{In plot_dfield, plot_bar and plot_matrix:}\cr
    Optional sorted numeric vectors of bin midpoints or breakpoints.\cr
    (Refer to the details section for plot_dfield).\cr

    \bold{In plot_cfield, plot_surface, plot_vec and plot_vec3:}\cr
    Optional sorted numeric vectors of grid point coordinates.\cr
    Note, I recommend that the lengths of x and y are different for 3d vector fields.\cr

    \bold{In plot_tricontour and plot_trisurface:}\cr
    Ignored.\cr

    \bold{In plot_isosurface:}\cr
    Same as plot_cfield, except that:\cr

    (1) They can also be iso-lists of such vectors.\cr
    (2) They're required, if fv is an iso-list, and the corresponding array dimension, has different sizes.\cr

    Refer to the details section for plot_isosurface.\cr

    \bold{In plot_cfield3:}\cr
    Same as plot_cfield, except that z is the vertical slide/slice position.\cr

    Note that where x, y and z are missing, they default to:\cr
    1:nrows, 1:ncols, 1:nsheets (the third array dimension) or 1:nslides.}
\item{fv}{
    \bold{In plot_dfield, plot_bar and plot_matrix:}\cr
    A numeric matrix of function (bin) values.\cr
    By default, if fv has row/column names, they will be used.\cr

    \bold{In plot_cfield and plot_surface:}\cr
    A numeric matrix of function (point) values.\cr

    \bold{In plot_tricontour and plot_trisurface:}\cr
    A numeric square matrix of function values.\cr
    Only the top-left triangle is used, including the diagonal.\cr

    \bold{In plot_isosurface}:\cr
    A numeric 3d array, or an iso-list of such arrays.\cr
    (Refer to the details section for plot_isosurface).\cr

    \bold{In plot_cfield3:}\cr
    A list of numeric matrices of function values.\cr
    Each matrix corresponds to one z value.\cr

    Note that in discretely-spaced plots, fv may contain missing values.\cr
    In other plots, missing values are not allowed.}
\item{dx, dy, dz}{
    \bold{In plot_vec:}\cr
    Numeric matrices, of equal size, giving the x and y components of a vector field.\cr
	
    \bold{In plot_vec3:}\cr
    Numeric 3d arrays, of equal size, giving the x, y and z components of a vector field.\cr

    In plot_vec, they may include missing values.\cr
    Note, I recommend that x-dim and y-dim are different for 3d vector fields.\cr
    (i.e. dim (dx)[1] != dim (dx)[2]).\cr
    This is to minimize possible over plotting.}
\item{fb}{Optional numeric vector of function values (or "levels"), for contours/isosurfaces.}
\item{fq}{
    Alternative way of specifying fb, above.\cr
    Numeric vector of probabilities (between zero and one), for quantiles of fv.}
\item{P}{Numeric, between zero and one, giving the proportion of (highest magnitude) vectors to plot.}
\item{main, xlab, ylab, zlab}{Optional strings, main/axes titles.}
\item{transpose}{Logical, if true, transpose fv, along with the any bin labels and colors.}
\item{contours}{Logical, include contour lines.}
\item{grid}{
    Logical, include grid lines.\cr
    For plot_matrix, defaults to true, if both the number of rows and columns don't exceed twenty.}
\item{wire.frame}{Logical, include the wire.frame lines.}
\item{gradient.shading}{Logical, use gradient shading.}
\item{vectors}{Logical, include vector arrows.}
\item{heatmap}{Logical, include heatmap.}
\item{contour.labels}{Logical, include contour labels.}
\item{slide.labels}{Logical, include the slide labels.}
\item{bin.labels}{
    Logical, include bin labels.\cr
    For plot_matrix, defaults to true, if both the number of rows and columns don't exceed ten.}
\item{include.outermost.vectors}{
    Logical, plot all vector arrows.\cr
    Otherwise (the default), exclude the outermost rows and columns.}
\item{texture}{Logical, if true, add a small amount of random variation to isosurface colors.}
\item{add}{Logical, if true, add contours/heatmap to an existing plot.}
\item{axes}{Logical vector of length one or two, if true, plot reference arrows or axis ticks with labels.}
\item{z.axis}{Logical, if true, include a z axis.}
\item{ref.arrows}{Logical vector of length one or two, if true, plot reference arrows, if false plot axis labels.}
\item{z.ref.arrow}{Logical, if true, plot a z reference arrow, if false, plot z axis labels.}
\item{reverse}{Logical vector of length one or two, if true, reverse the x and y axes.}
\item{z.reverse}{Logical, if true, reverse the z axis.}
\item{swap.sides}{Logical vector of length one or two, if true, swap the axes.}
\item{xyrel}{
    Single character, either "f", "s" or "m".\cr
    "f" produces a plot with a fixed aspect ratio of one,\cr
    "s" produces a square plot, and "m" a maximized plot.\cr\cr
    The default is determined by the \link{test.xyrel} function.}
\item{continuous.axes}{
	Logical of length one or two.\cr
	If true, plot the axes in the same was as continuous plots.}
\item{zlim}{Optional length-2 numeric vector, the vertical range, corresponding to function value.}
\item{ncontours, nsurfaces}{Integer, the number of contours/surfaces.\cr Ignored, if fb or fq supplied.}
\item{nested, maximal}{
	nested\cr
	Logical value, if true (the default), assume that the isosurfaces are nested.\cr\cr
	maximal\cr
	Logical value, if true, assume that (nested) isosurfaces are centered around local maxima, if false, assume that they're centered around local minima.\cr\cr
	These determine the default fb values, including their order.\cr
	(Ascending for low-valued focal points, and descending for high-valued focal points).}
\item{emph}{What to emphasize:\cr
    "n" (for nothing).\cr
    "b" for both (low and high regions).\cr
    "l" for low regions.\cr
    "h" for high regions.\cr
    "B", "L" or "H" for the same but with a stronger effect.}
\item{clabs}{Optional character vector of contour labels.\cr
    Ignored, unless contour.labels is true.}
\item{blabs}{
    Optional character matrix of bin labels.\cr
    Its dimensions need to match fv.\cr
    Ignored, unless bin.labels is true.\cr
    Defaults to the values of fv.}
\item{xat, yat}{Optional numeric vectors, the x and y axes tick/label points.}
\item{xlabs, ylabs}{
    Optional character vectors, the x and y axes tick labels.\cr
    In 3d plots, ignored unless axes is true and arrows is false.}
\item{zlabs}{Optional character vectors, the slide labels.}
\item{nhl}{Integer, the number of header lines.}
\item{arrowh.length, arrowh.width}{Numerics, arrow head length and width, in mm.}
\item{raster}{Logical, if true, use raster-based graphics for the heatmap.\cr
    Note that it's possible the default value (false) may change, in the future.\cr
    Please refer to the subsection on raster-based heatmaps in the details section, if you're interested in setting this to true.}
\item{contour.color}{Character vector (or R color strings) of length one or with the same length as the number of contours, giving the contour line colors.}
\item{grid.color}{String (R color string), giving the grid line color.}
\item{wire.color}{Sring (R color string), giving the wire frame color.}
\item{colf}{A litmus object (or equivalent color function), refer to details section for color functions.}
\item{colff}{A litmus-fitting function, refer to details section for color functions.}
\item{top.color, side.color}{
    Strings (R color strings) giving the top and side colors of bars.\cr
    Also, can be character matrices.\cr
    If matrices, then their dimensions need to match fv.}
\item{iso.colors}{
    Character vector (R color strings), giving the (initial) isosurface colors.}
\item{colors}{
    Optional character matrix (R color strings), for the bin colors.\cr
    Its dimensions need to match fv.}
\item{arrow.color, fill.color}{Strings (R color strings), arrow line and fill colors.}
\item{head.color, mid.color, tail.color}{Strings (R color strings), the arrow colors.}
\item{theme}{String, the name of a supported theme.\cr
    If missing, default color values are determined by global options.\cr\cr
    Refer to \link{set.bs.theme}.}
\item{hcv}{Logical, if true, use the high color variation option.}
\item{\dots}{Ignored.}
}
\details{
\bold{FUNCTIONS OF TWO DISCRETE VARIABLES}\cr
\bold{(AND DISCRETELY-SPACED SCALAR FIELDS)}\cr
(plot_dfield and plot_bar)\cr

In plot_dfield and plot_bar, the main argument is fv, which is a numeric matrix of function values.\cr
(Each fv value is a bin value).

Increasing row indices (i.e. moving down the matrix) correspond to increasing x values, and increasing column indices (i.e. moving right across the matrix) correspond to increasing y values.

x and y arguments, are numeric vectors of midpoints or breakpoints, of the bins.

If midpoints, then the length of x needs to be equal to the number of rows and the length of y needs to equal the number of columns. If breakpoints, then the length of x needs to be equal to the number of rows plus one, and the length of y needs to equal the number of columns plus one.\cr\cr


\bold{FUNCTIONS OF TWO CONTINUOUS VARIABLES}\cr
\bold{(AND CONTINUOUSLY-SPACED SCALAR FIELDS)}\cr
(plot_cfield and plot_surface)\cr

In plot_cfield and plot_surface, the main argument is fv, which is also a numeric matrix of function values.\cr
(However, each fv value is a point value, and the resulting heatmap is for interpolated bin values).

As with the discretely spaced case, increasing row indices (i.e. moving down the matrix) correspond to increasing x values, and increasing column indices (i.e. moving right across the matrix) correspond to increasing y values.

However, optional x and y arguments, give the coordinates of the points.

The length of x needs to be equal to the number of rows, and the length of y needs to equal the number of columns.\cr\cr

\bold{TRIANGULAR PLOTS}\cr
(plot_tricontour and plot_trisurface)\cr

These functions are similar to plot_cfield and plot_surface, except that the fv matrix, needs to be square, and the x and y coordinates are ignored.\cr\cr

\bold{ISOSURFACE PLOTS}\cr
(plot_isosurface)\cr

THIS FUNCTION REQUIRES THE misc3d PACKAGE TO BE INSTALLED AND LOADED\cr
(There's no requirement for the package to be attached).

The number of isosurfaces is determined by nsurfaces.\cr
Or by fb or fq, if provided.

The order of fb is important.\cr
If the isosurfaces are nested, and the default colors are used, then the fb values should be in order of the innermost to the outermost.

This package supports two types of isosurface plots, simple isosurface plots, and multi-array isosurface plots.

For simplicity, an "iso-list" is shorthand, for a list, with the same length as the number of isosurfaces.\cr

\emph{SIMPLE ISOSURFACE PLOTS}\cr

In simple isosurface plots, fv is a numeric 3d array of function values.

Increasing along the first array dimension corresponds to increasing x values, increasing along the second array dimension corresponds to increasing y values, and increasing along the third array dimension corresponds to increasing z values.

There are optional x, y and z arguments, which are numeric vectors, giving the coordinates of the points.

The length of x needs to be equal the x-dim size, the length of y needs to equal the y-dim size, and the length of z needs to equal the z-dim size, where x-dim, y-dim and z-dim refer to dim (fv)[1], dim (fv)[2] and dim (fv)[3], respectively.\cr

\emph{MULTI-ARRAY ISOSURFACE PLOTS}\cr

In multi-array isosurface plots, fv is an iso-list of numeric 3d arrays.\cr
If the number of arrays is not equal to two (the default value for nsurfaces), then you will need to set nsurfaces, fb or fq.

x, y, z can be numeric vectors or iso-lists of numeric vectors.

If coordinates are iso-lists and fv is an iso-list, then their sizes need to match, surface-wise.\cr
i.e.

length (x [[k]]) needs to equal dim (fv [[k]])[1], for all k\cr
length (y [[k]]) needs to equal dim (fv [[k]])[2], for all k\cr
length (z [[k]]) needs to equal dim (fv [[k]])[3], for all k\cr
where k is the kth isosurface.\cr

\bold{OTHER 3D-BASED CONTOUR PLOTS AND HEATMAPS}\cr
(plot_cfield3)\cr

plot_slides is similar to plot_cfield, but produces a set of overlapping slides/slices.

Here, fv is a list of numeric matrices.\cr
(One matrix for each slide).

And z, is the coordinate vector for the slides/slices.\cr
(One z value for each slide, and the corresponding fv matrix).

The function contains an emph (emphasis) argument.

Note that setting heatmap to false, will increase the rendering speed.\cr\cr


\bold{MATRIX VISUALISATION}\cr
(plot_matrix)\cr

The plot_matrix function is a wrapper for plot_dfield.

By default, it transposes the matrix, and then calls plot_dfield with different default values.\cr
This causes the resulting plots to have the same orientation as matrices, in textual form.

Note that in principle, this transposes the matrix, twice.\cr
Because plot_dfield transposes the matrix, in terms of visual interpretation.\cr
Without transposed input, increasing row indices (going down) map to increasing x values (going right).

Note that if bin labels or bin colors are supplied, they're also transposed.\cr
All other arguments (x, y, etc) apply to the transposed matrix, not the original.\cr\cr


\bold{2D VECTOR FIELDS}\cr
(plot_vec)\cr

plot_vec is similar to plot_cfield, except that there are two dx and dy matrices, instead of a single fv matrix.

The color of the heatmap bins reflects the magnitude of the four surrounding vectors.\cr\cr


\bold{3D VECTOR FIELDS}\cr
(plot_vec3)\cr

plot_vec3 is similar to plot_vec.

Instead of two numeric matrices, we have three numeric 3d arrays, including dz.\cr

Increasing along the first array dimension corresponds to increasing x values, increasing along the second array dimension corresponds to increasing y values, and increasing along the third array dimension corresponds to increasing z values.

Note that this function has had less testing than other plotting functions in this package.\cr\cr


\bold{3D COORDINATE SYSTEM}\cr

Currently, 3d plots use a diamond-like orthographic/dimetric projection, with a fixed viewing angle.

By default, (min (x), min (y) ) is at the bottom center.
And increasing x values, run diagonally up-right, and increasing y value run diagonally up-left.

It's not possible to rotate the plots, but the x and y axis can be reversed.

In functions of two variables, the z coordinate represents the function value, and increasing z values run up.\cr\cr

\bold{RASTER-BASED HEATMAPS}\cr

Raster-based heatmaps, are rendered via the graphics::rasterImage function, so please refer to that function, for further technical details. The plot_dfield function sets the interpolate argument in rasterImage to false, and the plot_cfield functions sets it to true. However, it's possible that results may vary across graphics devices.

Note that this feature is relatively new, and has not had extensive testing.

Currently, only the first and last x and y values are used, in the rendering step.\cr
(However, you still need the whole sequences, because the x and y values are used in other parts of the code).

Which means that, x and y, if provided, need to be equally-spaced.\cr
There's no test for this, and plots with unequally-spaced x and y values will produce incorrect output.

Raster-based heatmaps are likely to render faster, which is especially noticeable for high resolution heatmaps.\cr\cr

\bold{PLOT COLORS}\cr

The color function (colf) can be any function that maps a numeric vector/matrix (as its first argument) to a character vector/matrix of R colors, however, I recommend using litmus objects. The litmus-fitting function (colff), is a function that maps a numeric vector (also, as its first argument) to a valid color function, however, I recommend using a function that returns a litmus object. 

Assuming that colf, colff and colors are missing, a litmus-fitting function is determined by calling one of the "st" functions.

Refer to \link{Standardized Palette Interface}.

The default litmus-fitting function will be determined by the theme argument, if the theme argument is provided, otherwise, it will be determined by global options.

Then the litmus object is fitted to the input values (usually, fv), then the colors are computed from the litmus object.

Similar principles apply to top.color, side.color and iso.colors, except that the "st" functions returns the colors rather than litmus-fitting functions.

In some cases, it may be useful to wrap a predefined litmus-fitting function.\cr
e.g. To use nonstandard hot and cold colors, wrap the hot.and.cold.fit function, then set the colff argument to the wrapper function.

Opaque color functions are preferable in 2d heatmaps.\cr\cr

\bold{ADDITIONAL NOTES}\cr

If using plot_bar, plot_surface or plot_trisurface to plot constant or near-constant values, set zlim, for a better result.\cr
Noting that functions that are constant in theory, may produce non-constant values when computed via floating point arithmetic.

In applied mathematics and applied statistics, you may want to set z.axis=TRUE and ref.arrows=FALSE (in 3d plots), and xyrel="m" (in 2d plots).\cr
The default for reference arrows, can also be set via global options.

Often, it may be useful to create wrapper functions, that set the default argument values, accordingly.

There's no guarantee that default contour lines will be suitable.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{Functional Versions}\cr
Which take a function as their main argument.

\link{litmus}, \link{litmus.fit}

\link{Predefined Litmus Objects}, \link{Predefined Litmus-Fitting Functions}\cr
With predefined colors.

\link{Standardized Palette Interface}, \link{Global Options}
}
\examples{
#NOTE THAT THERE ARE MORE EXAMPLES IN:
#(1) THE PACKAGE VIGNETTE
#(2) THE HELP PAGE FOR FUNCTIONAL VERSIONS

#discretely-spaced example
fv <- matrix (1:100, 10, 10, byrow=TRUE)

plot_dfield (,,fv)
plot_bar (,,fv)
plot_matrix (,,fv)

#continuously-spaced example
x <- y <- seq (-15.5, 15.5,, 40)
fv <- outer (x, y, rotated.sinc)

plot_cfield (x, y, fv, fb=0, hcv=TRUE)
plot_surface (x, y, fv)
}
