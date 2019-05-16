\name{rd_2_plots_continuous}
\alias{plot2d.contour}
\alias{plot3d.surface}
\alias{plot3d.surf}
\title{Plots of Functions of Two Continuous Variables}
\description{Plots (in 2D and 3D) of functions (represented by matrices) of two continuous variables.}
\usage{
plot2d.contour (x, y, z, zb,
    contours=TRUE, heat.map=TRUE,
    main, xlab="x", ylab="y",
    colv.1, colv.2, contrast=0,
    pty="s", \dots)

plot3d.surface (x, y, z,
    main, xlab="x", ylab="y",
    colv.1, colv.2, contrast=0,
    zlim, \dots)

#deprecated, do not use
plot3d.surf (\dots)
}
\arguments{
\item{x}{An optional sorted vector of x values.}
\item{y}{An optional sorted vector of y values.}
\item{z}{A matrix of point values.}
\item{zb}{A vector of contour values.}
\item{contours}{If true (the default), plot contour lines.}
\item{heat.map}{If true (the default), plot a heat map.}
\item{main}{.}
\item{xlab}{.}
\item{ylab}{.}
\item{colv.1}{An HCL color vector.}
\item{colv.2}{An HCL color vector.}
\item{contrast}{A numeric scalar, giving the relative contrast, currently in the interval [-1, 0].}
\item{pty}{.}
\item{zlim}{The z axis limits.}
\item{\dots}{Other arguments for par().}
}
\details{
If x and y values are supplied then their lengths should be equal to the dimensions of z.

For plot2d.contour, colors are interpolated between colv.1 and colv.2 based on the values of z. For plot3d.surface, colors are interpolated based on the slopes of z.

By default, colors are determined by global options.
}
\examples{
x = y = 1:30
f = function (x, y) x ^ 2 + y ^ 2
z = outer (x, y, f)
plot3d.surface (,,z)
}
