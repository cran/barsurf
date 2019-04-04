\name{rd_1_plots_discrete}
\alias{plot2d.cell}
\alias{plot3d.bar}
\title{Plots of Functions of Two Discrete Variables}
\description{Plots (in 2D and 3D) of functions (represented by matrices) of two discrete variables, or plots of other matrices.}
\usage{
plot2d.cell (xb, yb, z,
    grid.lines=TRUE,
    main, xlab="x", ylab="y", xlabs, ylabs,
    colv.1, colv.2, colv.na,
    pty="s", \dots)

plot3d.bar (xb, yb, z,
    main, xlab="x", ylab="y", xlabs, ylabs,
    colv.1, colv.2, colvs.sms, reverse=FALSE,
    zlim, \dots)
}
\arguments{
\item{xb}{An optional sorted vector of x breakpoints.}
\item{yb}{An optional sorted vector of y breakpoints.}
\item{z}{A matrix of cell values.}
\item{grid.lines}{If true (the default), plot grid lines.}
\item{main}{.}
\item{xlab}{.}
\item{ylab}{.}
\item{xlabs}{The x axis tick labels.}
\item{ylabs}{The y axis tick labels.}
\item{colv.1}{An HCL color vector.}
\item{colv.2}{An HCL color vector.}
\item{colv.na}{An HCL color vector.}
\item{colvs.sms}{A vector (or matrix, with row vectors), giving the colors of the submatrices.}
\item{pty}{.}
\item{reverse}{If true, apply colors to submatrices in reverse order.}
\item{zlim}{The z axis limits.}
\item{\dots}{Other arguments for par().}
}
\details{
The matrix can be either a standard matrix object or a nested matrix object.

If xb and yb values are supplied then their lengths should be one more than the dimensions of z.

For plot2d.cell, colors are interpolated between colv.1 and colv.2 based on the values of z. For plot3d.bar, colv.1 is the top of the bars, and colv.2 is the front of the bars.

By default, colors are determined by global options.
}
\examples{
z = outer (1:10, 1:10, "+")
plot3d.bar (,,z)
}
