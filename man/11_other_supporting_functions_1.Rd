\name{11_other_supporting_functions_1}
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
fv <- matrix (1:20, 2, 10)
test.xyrel (,,fv)
}
