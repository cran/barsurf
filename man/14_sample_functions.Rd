\name{14_sample_functions}
\alias{rotated.sinc}
\alias{bispherical.dist}
\alias{concentric.field}
\title{Sample Functions}
\description{Functions to produce sample scalar and vector fields.}
\details{
The rotated.sinc function was adapted from the graphics::persp examples.

The bispherical.dist function gives the smaller of distances from two points at:\cr
(-1, 1, 0)\cr
(1, -1, 0)

The concentric.field functions generates a vector field with circular flow, and highest magnitude at r=1, where r is the distance from the origin.
}
\usage{
#scalar-valued (theoretically)
rotated.sinc (x, y)
bispherical.dist (x, y, z)

#vector-valued (theoretically)
concentric.field (x, y)
}
\arguments{
\item{x, y, z}{Numeric vectors, where the functions are evaluated.}
}
\value{
The (theoretically) scalar-valued functions return a numeric vector.

The (theoretically) vector-valued functions return a list containing two numeric vectors.
}
\examples{
rotated.sinc (0, 0)
bispherical.dist (0, 0, 0)
concentric.field (c (0, 1), 0)
}
