\name{15_fitting_litmus_objects_to_data}
\alias{litmus.fit}
\alias{heat.litmus.fit}
\alias{hot.and.cold.fit}
\alias{blue.litmus.fit}
\alias{green.litmus.fit}
\alias{blue.litmus.fit.hcv}
\alias{green.litmus.fit.hcv}
\alias{blue.litmus.fit.flow}
\alias{green.litmus.fit.flow}
\alias{heat.litmus.fit.lum}
\alias{gold.litmus.fit.lum}
\alias{blue.litmus.fit.lum}
\alias{green.litmus.fit.lum}
\alias{purple.litmus.fit.lum}
\alias{rainbow.litmus.fit}
\alias{rainbow.litmus.fit.2}
\alias{glass.rainbow.fit}
\title{Fit Litmus Objects to Data}
\description{Functions to fit litmus objects to vectors of data.}
\details{
Refer to the litmus function for background information.

The litmus.fit function constructs a litmus object.

Given n colors, it computes a length-n vector of knots computed from a vector of data.

If equalize is zero, the knots are equally spaced from the lowest x value to the highest. If equalize is one, then knots are selected, such that there's an approximately equal number of points between each pair of knots. And equalization values between zero and one result in an intermediate effect.

Note that high equalize values (higher than the default) may cause color interpolation to appear less smooth.

In general, it's easiest to wrap the litmus.fit function inside another function, which defines the color space and the colors.

This package defines a range of wrapper functions, for the heat, blue, green and purple color themes.
}
\usage{
########################################
#general cases
########################################
litmus.fit (x, colvs, \dots,
    color.space="sRGB", reverse=FALSE, equalize=0.85, na.color="#FFFFFF")

########################################
#wrappers, with predefined colors
########################################
#opaque
#high impact
heat.litmus.fit (x, \dots, reverse=FALSE, equalize=0.85)
hot.and.cold.fit (x, xb=0)

#opaque
#interpolate over hue
blue.litmus.fit (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit (x, \dots, reverse=FALSE, equalize=0.85)

#opaque
#dark-color -> light-color -> white
blue.litmus.fit.hcv (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit.hcv (x, \dots, reverse=FALSE, equalize=0.85)

#opaque
#interpolate over chroma and lum
blue.litmus.fit.flow (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit.flow (x, \dots, reverse=FALSE, equalize=0.85)

#semitransparent
#interpolate over chroma and lum
heat.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
gold.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
blue.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
purple.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)

#opaque
#adapted from colorspace::rainbow_hcl
rainbow.litmus.fit (x, \dots, c=42.5, l=75, start=65, end=315, equalize=0.85)
rainbow.litmus.fit.2 (x, \dots, c=50, l=70, start=0, end=360, equalize=0.85)

#semitransparent
glass.rainbow.fit (x, alpha=0.3, \dots, c=42.5, l=62.5, start=42.5, end=260,
    equalize=0.85)
}
\arguments{
\item{x}{A numeric vector.}
\item{xb}{Numeric, giving the breakpoint between "hot" and cold".}
\item{colvs}{A 3-column or 4-column numeric matrix, where each row is one color vector, and the optional fourth column is alpha values.}
\item{color.space}{A string giving the color space, refer to the details section for litmus objects.}
\item{reverse}{Logical, reverse the order of the colors.}
\item{equalize}{Numeric, between zero and one, refer to details.}
\item{na.color}{A single string representing an R color.}
\item{c, l, start, end}{Same as colorspace::rainbow_hcl.}
\item{alpha}{A numeric vector giving the alpha component. If it has two or more values, then each alpha value is assigned to each knot.}
\item{\dots}{Ignored.}
}
\value{
All functions return litmus objects, except the hot.and.cold function which returns a mlitmus object.
}
\examples{
rainbow.litmus.fit.3 <- function (x)
{   colvs <- cbind (c (110, 170, 230, 290), 42.5, 75)
    litmus.fit (x, colvs, color.space="HCL")
}

x <- rnorm (200)
colf <- rainbow.litmus.fit.3 (x)

#evaluate
colf (min (x) )
colf (mean (x) )
colf (max (x) )
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{\link{litmus}}
