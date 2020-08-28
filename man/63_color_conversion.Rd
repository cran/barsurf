\name{63_color_conversion}
\alias{map.color}
\alias{map.color.3}
\title{Color Conversion}
\description{Convert color vectors between different color spaces.}
\usage{
map.color (colvs, string=FALSE, \dots, from="sRGB", to="HCL", correction=TRUE)
map.color.3 (x, y, z, alpha, string=FALSE, \dots, from="sRGB", to="HCL", correction=TRUE)
}
\arguments{
\item{colvs}{
	A length-3 or length-4 numeric vector, or 3-column or 4-column numeric matrix.\cr
	They give the color components for a particular color space, determined by the from argument.}
\item{x, y, z, alpha}{
	Same as colvs, except separate vectors for each color component.\cr
	If alpha is missing, it's equivalent to a 3-column matrix, and if alpha supplied, it's equivalent to a 4-column matrix.}
\item{string}{Logical, if true, return (R color) strings, if false, return another colvs matrix.}
\item{from, to}{
	Strings giving the name of the input and output color spaces.\cr
	This includes "XYZ", "RGB", "LAB", "polarLAB", "HSV", "HLS", "LUV" and "polarLUV", in addition to "HCL".\cr\cr
	The to color space is ignored, if string is true.}
\item{correction}{
	Logical, if true, apply a correction to sRGB values.\cr
	Ignored, unless string is false and the to color space is sRGB.}
\item{\dots}{Ignored.}
}
\details{
These functions are wrappers for functions in the colorspace package.

At the time of writing, there are problems converting HCL (and PolarLUV) to HSV and HLS.
}
\value{Either a character vector of R color strings, or a 3-column or 4-column matrix of color vectors.}
\examples{
map.color.3 (0, 0, c (0.25, 0.75) )
map.color.3 (0, 0, c (0.25, 0.75),, TRUE)
}
