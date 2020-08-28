\name{12_functional_versions}
\alias{Functional Versions}
\alias{plotf_dfield}
\alias{plotf_bar}
\alias{plotf_cfield}
\alias{plotf_surface}
\alias{plotf_tricontour}
\alias{plotf_trisurface}
\alias{plotf_isosurface}
\alias{plotf_cfield3}
\alias{plotf_vec2}
\alias{plotf_vec3}
\title{Plotting Functions for Function Objects}
\description{
These functions wrap the main plotting functions.\cr
They take a suitable function object, along with xlim/ylim/etc, and compute the x/y/fv/etc values.

Refer to \link{Main Plotting Functions}.}
\usage{
plotf_dfield (f, xlim, ylim=xlim, \dots)
plotf_bar (f, xlim, ylim=xlim, zlim, \dots)

plotf_cfield (f, xlim, ylim=xlim, \dots, n=30, nc = max (n, 60), nh=n)
plotf_surface (f, xlim, ylim=xlim, zlim, \dots, n=30)

plotf_tricontour (f, \dots, n=30)
plotf_trisurface (f, \dots, n=30)

plotf_isosurface (f, xlim, ylim=xlim, zlim=xlim, \dots,
    nsurfaces=2, fb, fq,
    nested=TRUE, maximal, panel.contours=TRUE,
    base.contours=panel.contours, rear.contours=panel.contours,
    pconstants,
    n)

plotf_cfield3 (f, xlim, ylim=xlim, zlim=xlim, \dots,
    nslides=6, z.reverse=FALSE, z, n=30)

plotf_vec2 (vf, xlim, ylim=xlim, \dots, n=20, nv=n, nh = max (n, 40) )
plotf_vec3 (vf, xlim, ylim=xlim, zlim=xlim, \dots, n = c (20, 22, 16) )
}
\arguments{
\item{f}{
    In plotf_dfield, plotf_bar, plotf_cfield and plotf_surface, a numeric-valued function of two variables.\cr
    In plotf_tricontour and plotf_trisurface, a numeric-valued function of two weight variables, that (along with a third implicit variable) sum to one.\cr
    In plotf_isosurface and plotf_cfield3, a numeric-valued function of three variables.}
\item{vf}{
    In plotf_vec2, a function of two variables that returns a two-column matrix, giving the dx and dy values.\cr
    In plotf_vec3, a function of three variables that returns a three-column matrix, giving the dx, dy and dz values.}
\item{xlim, ylim, zlim}{
    In plotf_dfield and plotf_bar, a length-two integer vector, giving the plotting ranges.\cr
    In other plots, a length-two numeric vector, giving the plotting ranges.\cr
    If descending, reverses the orientation of the plot.\cr\cr
    Note that in plotf_cfield3, zlim is ignored if z supplied.\cr
    And currently, limits need to be ascending for 3d vector fields.}
\item{nsurfaces, fb, fq}{
    The number of isosurfaces, and function values (or ``levels''), for isosurfaces.\cr
    Refer to plot_isosurface for more information.}
\item{nested, maximal}{
	nested\cr
	Logical value, if true (the default), assume that the isosurfaces are nested.\cr\cr
	maximal\cr
	Logical value, if true, assume that (nested) isosurfaces are centered around local maxima, if false, assume that they're centered around local minima.\cr\cr
	These determine the default fb values, including their order.\cr
	(Ascending for low-valued focal points, and descending for high-valued focal points).\cr\cr
    	The nested argument also determines the default n values.}
\item{panel.contours, base.contours, rear.contours}{Logical, include contour lines on the base or rear panels of the plot.}
\item{pconstants}{
    Optional length-3 numeric vector of panel constants.\cr
    Defaults to the midpoints of xlim, ylim and zlim.\cr
    Refer to details and examples.}
\item{nslides}{
    Integer, the number of slides.\cr
    Ignored, if z supplied.}
\item{z.reverse}{
    Logical, reverse the z axis.\cr
    Ignored, unless z supplied.\cr
    (i.e. If z is missing, zlim determines the vertical orientation).}
\item{z}{
	Optional numeric vector, of z coordinates.\cr
	If supplied, there will be one slide for each z value.}
\item{n}{
	In most plots, an integer of length one or two, giving the number of bin/grid points, in each x/y direction.
	In plotf_isosurface and plotf_vec3d, an integer of length one or three, giving the number of grid points, in each x/y/z direction.\cr\cr
	In plotf_isosurface, can also be a list of such vectors, one for each isosurface.\cr\cr
	Note, I recommend that the n values for x and y be different, for 3d vector fields.}
\item{nc, nv, nh}{Same as n, but computes separate input matrices, for contour lines (nc), vector arrows (nv) and heatmaps (nh).}
\item{\dots}{Additional arguments for the main plotting functions.}
}
\details{
These functions wrap the main plotting functions.\cr
i.e. plotf_isosurface calls plot_isosurface.

They take a suitable function object, along with xlim/ylim/etc, and compute the x/y/fv/etc values.\cr
i.e. plotf_surface takes a numeric-valued function of two variables, a computes the x and y vectors, along with the fv matrix.

If you need to plot different functions to those described by the f argument above, it may be possible to create a wrapper function.\cr\cr

\bold{ISOSURFACE PLOTS}\cr
(plotf_isosurface)\cr

THIS FUNCTION REQUIRES THE misc3d PACKAGE TO BE INSTALLED AND LOADED\cr
(There's no requirement for the package to be attached).

If the nested argument is true, different n sizes are used for each isosurface.\cr
The resolution of the fv array is highest for the first isosurface, and each fv array has a progressively lower resolution.
Resulting plots should result in smaller file sizes, and render more quickly.

Optionally, 2D contour lines may be added to the base panel and rear panels.\cr
This needs a vector of panel constants, which defaults to the xlim/ylim/zlim midpoints.\cr
The function (of three variables), f, is called three times, holding one variable constant, each time.

The third constant is for the base panel contours.\cr
And the first and second constants, are for the right-rear and left-rear panel contours, respectively.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{Main Plotting Functions}

\link{Global Options}

\link{litmus}, \link{litmus.fit}

\link{Predefined Litmus Objects}, \link{Predefined Litmus-Fitting Functions}\cr
With predefined colors.
}
\examples{
##############################
#functions of two variables
#(discretely-spaced)
##############################
df <- function (x, y, n=10, p=0.5)
    dbinom (x, n, p) * dbinom (y, n, p)

plotf_dfield (df, c (0, 10) )
plotf_bar (df, c (0, 10) )

##############################
#functions of two variables
#(continuously-spaced)
##############################
plotf_cfield (rotated.sinc, c (-15.5, 15.5), fb=0, n=40, hcv=TRUE)
plotf_surface (rotated.sinc, c (-15.5, 15.5), n=40)

##############################
#triangular functions
##############################
tf <- function (w1, w2, w3 = 1 - w1 - w2)
    (w1 - 1 / 3)^2 + (w2 - 1 / 3)^2 + (w3 - 1 / 3)^2

plotf_tricontour (tf, xlab="w1", ylab="w2")
plotf_trisurface (tf, xlab="w1", ylab="w2")

##############################
#functions of three variables
##############################
plotf_isosurface (bispherical.dist, c (-3, 3),, c (-2, 2) )
plotf_cfield3 (bispherical.dist, c (-3, 3),, c (-2, 2), emph="l")

##############################
#vector fields
##############################
plotf_vec2 (circular.field, c (-1.5, 1.5) )
plotf_vec3 (plughole.field, c (-1.5, 1.5),, c (-0.5, 1) )
}
