\name{61_check_xy_relationship}
\alias{test.xyrel}
\title{Test Axis Relationship}
\description{Estimate the (plotting) relationship between the x and y axes.}
\details{
This function is designed to work with the xyrel argument in the main plotting functions.

It returns "f" (for a fixed aspect ratio of one) or "m" (for maximized).

If the ratio between the x-size and the y-size is between 0.1 and 10, it will return "f".\cr
Otherwise, it returns "m".

If both x any y are missing, the "size" refers to the dimensions of the matrix.\cr
(So, returns "f" for up to ten times more columns than rows, or vice versa).

If both x and y are supplied, the "size" refers to xlim and ylim, computed from the ranges of x and y.

If one is supplied but the other is not, then "m" is returned.
}
\usage{
test.xyrel (x, y, fv)
}
\arguments{
\item{x, y}{Optional numeric vectors.}
\item{fv}{A numeric matrix.}
}
\value{
Refer to details.
}
\examples{
fv1 <- matrix (1:40, 2, 20)
fv2 <- matrix (1:50, 2, 25)

test.xyrel (,,fv1)
test.xyrel (seq (0, 1,, 2), seq (0, 20,, 20), fv1)

test.xyrel (,,fv2)
test.xyrel (seq (0, 1,, 2), seq (0, 1,, 25), fv1)
}
