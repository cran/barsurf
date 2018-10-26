\name{barsurf}
\alias{plot3d.empty}
\alias{plot3d.bar}
\alias{plot3d.surf}
\title{Bar Plots and Surface Plots in 3d}
\description{Implements plots of discrete functions (bar plots) and continuous functions (surface plots) in 3D. Uses orthographic projection, a nonstandard fixed viewing angle, does not support detailed axes and is designed to be relatively minimal and simple, from a programming perspective. However, does support mixed bar sizes and multiple colors. Refer the vignette for better examples}
\usage{
plot3d.empty ()
plot3d.bar (x, y, z,
	main, xlab="x", ylab="y",
	col1=rgb (0, 0.8, 0.4), col2=rgb (0.5, 1, 0.7),
	zlim)
plot3d.surf (x, y, z,
	main, xlab="x", ylab="y",
	col1=rgb (0, 0.8, 0.4), col2="white",
	zlim)
}
\arguments{
\item{x}{An optional vector of sorted x values. For bar plots, length(x) should equal nrow(z)+1. For surface plots, length(x) should equal nrow(z).}
\item{y}{An optional vector of sorted y values. For bar plots, length(y) should equal ncol(z)+1. For surface plots, length(y) should equal ncol(z).}
\item{z}{A matrix of z values. Increasing row indices correspond to increasing x values and increasing column indices correspond to increasing y values.}
\item{main}{An optional main title.}
\item{xlab}{The x label, defaults to x.}
\item{ylab}{The y label, defaults to y.}
\item{zlim}{An optional z range for the plot.}
\item{col1}{The first color. For bar plots, may be a matrix.}
\item{col2}{The second color. For bar plots, may be a matrix.}
}
\details{Bar plots use two colors. The first color is the top of the bars and the second color is the front of the bars. Surface plots interpolate between two colors. Flatter regions are closer to the first color and steeper regions are closer to the second color.}
\examples{
#bar plot in 3d
#note the two commas
x = y = 1:4
f = function (x, y) x ^ 2 + y ^ 2
z = outer (x, y, f)
plot3d.bar (,,z)
}
