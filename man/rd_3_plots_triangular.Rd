\name{rd_3_plots_triangular}
\alias{plot2d.tricontour}
\alias{plot3d.trisurface}
\alias{lr2na}
\title{Plots of Functions over Triangular Domains}
\description{Plots (in 2D and 3D) of functions over triangular domains.}
\usage{
plot2d.tricontour (x, y, z, zb,
    contours=TRUE, heat.map=TRUE,
    main, xlab="x", ylab="y",
    colv.1, colv.2,
    pty="s", \dots)

plot3d.trisurface (x, y, z,
    main, xlab="x", ylab="y",
    colv.1, colv.2,
    zlim, \dots)

lr2na (z)
}
\arguments{
\item{x}{Ignored.}
\item{y}{Ignored.}
\item{z}{A square matrix of point values.}
\item{zb}{A vector of contour values.}
\item{contours}{If true (the default), plot contour lines.}
\item{heat.map}{If true (the default), plot a heat map.}
\item{main}{.}
\item{xlab}{.}
\item{ylab}{.}
\item{colv.1}{An HCL color vector.}
\item{colv.2}{An HCL color vector.}
\item{pty}{.}
\item{zlim}{The z axis limits.}
\item{\dots}{Other arguments for par().}
}
\details{
Only the upper left part of the matrix is used.

Refer to the details sections of plot2d.contour and plot3d.surface for more information.

Note that the x and y arguments are ignored.
}
\examples{
x = y = 1:30
f = function (x, y) x ^ 2 + y ^ 2
z = outer (x, y, f)
plot3d.surface (,,z)
}
