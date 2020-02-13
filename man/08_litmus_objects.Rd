\name{08_litmus_objects}
\alias{litmus}
\alias{litmus.spline}
\alias{litmus.heat}
\alias{litmus.blue}
\alias{litmus.green}
\alias{litmus.blue.hcv}
\alias{litmus.green.hcv}
\alias{litmus.blue.flow}
\alias{litmus.green.flow}
\alias{litmus.rainbow}
\alias{litmus.rainbow.2}
\alias{glass.rainbow}
\title{Litmus Objects}
\description{Color functions for heatmaps and surface plots.}
\details{
A litmus object maps a numeric vector to a character vector, representing R colors.

Color vectors are mapped from the input color space into sRGB color space.

And a set of cubic Hermite splines interpolates over each component.

Input color spaces include "XYZ", "RGB", "LAB", "polarLAB", "HSV", "HLS", "LUV" and "polarLUV" (from the colorspace package), in addition to "HCL" (which is the same as polarLUV, except that the color components are in the reverse order.). Input color vectors may have an alpha component, in which case, the mapping preserves it, such that the resulting sRGB colors will have the same alpha values.

There are two constructors, one for equally spaced knots and one for arbitrary knots.

The other functions (e.g. litmus.heat) are wrappers, that create litmus objects with particular colors.

Note that in theory, the interpolation is smooth, however, a smooth appearance (or a not so smooth appearance) is dependent on the choice of knots and colors. Similar consecutive colors tend to produce smoother looking results. In general, interpolating over hue, with constant chroma and luminescence produces the smoothest results, however, there are many situations where it's desirable for one area (within a plot) to appear brighter than others.

Note that if a litmus object is evaluated with x values outside the knots, then the function will return the first or last color.

Also note that it may be easier to construct litmus objects using the litmus.fit function, or one of it's wrapper functions.
}
\usage{
########################################
#general cases
########################################
#equally spaced knots
litmus (a=0, b=1, colvs,
    color.space="sRGB", na.col="#FFFFFF")

#arbitrary knots
litmus.spline (x, colvs,
    color.space="sRGB", na.col="#FFFFFF")

########################################
#wrappers, with predefined colors
########################################
#opaque
#high impact
litmus.heat (a=0, b=1, \dots, reverse=FALSE)

#opaque
#interpolate over hue
litmus.blue (a=0, b=1, \dots, reverse=FALSE)
litmus.green (a=0, b=1, \dots, reverse=FALSE)

#opaque
#high color variation
#dark-color -> light-color -> white
litmus.blue.hcv (a=0, b=1, \dots, reverse=FALSE)
litmus.green.hcv (a=0, b=1, \dots, reverse=FALSE)

#opaque
#interpolate over lum
litmus.blue.flow (a=0, b=1, \dots, reverse=FALSE)
litmus.green.flow (a=0, b=1, \dots, reverse=FALSE)

#opaque
#adapted from colorspace::rainbow_hcl
litmus.rainbow (a=0, b=1, \dots, c=42.5, l=75, start=65, end=315)
litmus.rainbow.2 (a=0, b=1, \dots, c=50, l=70, start=0, end=360)

#semi-transparent
glass.rainbow (a=0, b=1, alpha=0.3, \dots, c=42.5, l=62.5, start=42.5, end=260)
}
\arguments{
\item{a}{The lower limit (first knot).}
\item{b}{The upper limit (last knot).}
\item{x}{A numeric vector of knots (including the outermost values), which should be unique and ascending.}
\item{colvs}{A 3-column or 4-column matrix, where each row is one color vector, and the optional fourth column is alpha values.}
\item{color.space}{A string giving the color space, refer to details.}
\item{na.col}{A single string representing an R color.}
\item{reverse}{Logical, reverse the order of the colors.}
\item{c, l, start, end}{Same as colorspace::rainbow_hcl.}
\item{alpha}{A numeric vector giving the alpha component. If it has two or more values, then each alpha value is assigned to each color.}
\item{\dots}{Ignored.}
}
\value{
All functions return litmus objects.
}
\examples{
#bad example
colvs = matrix (c (
    0, 0, 0,
    0.75, 0, 0.25,
    0.25, 0, 0.75,
    1, 1, 1),, 3, byrow=TRUE)
colf = litmus (,,colvs)
plot (colf)

#better example
litmus.rainbow.3 = function (a=0, b=1)
{   colvs = cbind (c (110, 170, 230, 290), 42.5, 75)
    litmus (a, b, colvs, "HCL")
}
plot (litmus.rainbow.3 () )
}
\references{Refer to the vignette for an overview, references and better examples.}

