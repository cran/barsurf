\name{13_other_supporting_functions_3}
\alias{rgb2hsv}
\alias{hsv2rgb}
\alias{rgb2hcl}
\alias{hcl2rgb}
\title{Utility Functions for Color Conversion}
\description{Convert a color vector from sRGB space to HSV/HCL space, or vice versa.}
\details{
These functions are wrappers for functions in the colorspace package.

Note that these functions support single length-3 vectors only, however, other functions in this package support length-4 vectors with an alpha component.
}
\usage{
rgb2hsv (colv)
hsv2rgb (colv)

rgb2hcl (colv)
hcl2rgb (colv, correction=FALSE)
}
\arguments{
\item{colv}{A length-3 numeric vector of sRGB, HSV or HCL values.}
\item{correction}{Logical, correct the sRGB values, if they're outside the interval [0, 1].}
}
\value{
A length-3 vector.
}
\examples{
rgb2hcl (c (0, 0, 1) )
}
