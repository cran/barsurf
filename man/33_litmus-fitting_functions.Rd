\name{33_litmus-fitting_functions}
\alias{litmus.fit}
\title{Fit Litmus Objects to Data}
\description{Functions to fit litmus objects to vectors of data.}
\usage{
litmus.fit (x, colvs, \dots,
    color.space="sRGB", reverse=FALSE, equalize=0.85, na.color="#FFFFFF")
}
\arguments{
\item{x}{A numeric vector.}
\item{colvs}{A 3-column or 4-column numeric matrix, where each row is one color vector, and the optional fourth column is alpha values.}
\item{color.space}{A string giving the color space, refer to the details section for litmus objects.}
\item{reverse}{Logical, reverse the order of the colors.}
\item{equalize}{Numeric, between zero and one, refer to details.}
\item{na.color}{A single string representing an R color.}
\item{\dots}{Ignored.}
}
\details{
Refer to the litmus function for background information.

The litmus.fit function constructs a litmus object.

Given n colors, it computes a length-n vector of knots computed from a vector of data.

If equalize is zero, the knots are equally spaced from the lowest x value to the highest. If equalize is one, then knots are selected, such that there's an approximately equal number of points between each pair of knots. And equalization values between zero and one result in an intermediate effect.

Note that high equalize values (higher than the default) may cause color interpolation to appear less smooth.

In general, it's easiest to wrap the litmus.fit function inside another function.
}
\value{
All functions return litmus objects, except the hot.and.cold function which returns a mlitmus object.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{Main Plotting Functions}

\link{Functional Versions}\cr
Which take a function as their main argument.

\link{Global Options}

\link{litmus}, \link{mlitmus}

\link{Predefined Litmus Objects}, \link{Predefined Litmus-Fitting Functions}\cr
With predefined colors.
}
\examples{
rainbow.litmus.fit.3 <- function (x, ..., reverse=FALSE)
{   colvs <- cbind (c (110, 170, 230, 290), 42.5, 75)
    litmus.fit (x, colvs, color.space="HCL", reverse=reverse)
}

x <- rnorm (30)
colf <- rainbow.litmus.fit.3 (x)

plot (colf)

colf (seq (-1, 1,, 10) )
}
