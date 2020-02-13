\name{12_other_supporting_functions_2}
\alias{rgb2hcl}
\alias{hcl2rgb}
\title{Utility Functions for Color Conversion}
\description{Convert a color vector from sRGB space to HCL space, or vice versa.}
\details{
These functions are wrappers for functions in the colorspace package.

Note that these functions support single length-3 vectors only, however, other functions in this package support length-4 vectors with an alpha component.
}
\usage{
rgb2hcl (colv)
hcl2rgb (colv, correction=FALSE)
}
\arguments{
\item{colv}{A length-3 vector of sRGB or HCL values.}
\item{correction}{Correct the sRGB values, if they're outside the interval [0, 1].}
}
\value{
A length-3 vector.
}
\examples{
rgb2hcl (c (0, 0, 1) )
}
