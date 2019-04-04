\name{rd_4_color_conversion}
\alias{rgb2hcl}
\alias{hcl2rgb}
\title{Color Conversion}
\description{Convert a color vector from RGB space to HCL space, or vice versa.}
\usage{
rgb2hcl (colv)
hcl2rgb (colv, correction)
}
\arguments{
\item{colv}{A vector of RGB or HCL values.}
\item{correction}{Correct the RGB values, if they're outside the interval [0, 1].}
}
\examples{
rgb2hcl (c (0, 0, 1) )
}
