\name{31_litmus_objects}
\alias{litmus}
\alias{litmus.spline}
\title{Litmus Objects}
\description{Color functions for heatmaps and surface plots.}
\usage{
#equally spaced knots
litmus (a=0, b=1, colvs, \dots,
    color.space="sRGB", na.color="#FFFFFF")

#arbitrary knots
litmus.spline (cx, colvs, \dots,
    color.space="sRGB", na.color="#FFFFFF")
}
\arguments{
\item{a, b}{Numeric, the lower and upper limits.}
\item{cx}{A numeric vector of knots (including the outermost values), which should be unique and sorted.}
\item{colvs}{A 3-column or 4-column numeric matrix, where each row is one color vector, and the optional fourth column is alpha values.}
\item{color.space}{A string giving the color space, refer to details.}
\item{na.color}{A single string representing an R color.}
\item{\dots}{Ignored.}
}
\details{
A litmus object maps a numeric vector to a character vector, representing R colors.\cr
(Noting that these functions return litmus objects, so you call these functions, and then if necessary you can evaluate the resulting function).

Color vectors are mapped from the input color space into sRGB color space.\cr
And a set of cubic Hermite splines interpolates over each component.

Input color spaces include "XYZ", "RGB", "LAB", "polarLAB", "HSV", "HLS", "LUV" and "polarLUV" (from the colorspace package), in addition to "HCL" (which is the same as polarLUV, except that the color components are in the reverse order.). Input color vectors may have an alpha component, in which case, the mapping preserves it, such that the resulting sRGB colors will have the same alpha values.

There are two constructors, one for equally spaced knots and one for arbitrary knots.

Note that in theory, the interpolation is smooth, however, a smooth appearance (or a not so smooth appearance) is dependent on the choice of knots and colors. Similar consecutive colors tend to produce smoother looking results. In general, interpolating over hue, with constant chroma and luminescence produces the smoothest results, however, there are many situations where it's desirable for one area (within a plot) to appear brighter than others.

Note that if a litmus object is evaluated with x values outside the knots, then the function will return the first or last color.
}
\value{
Both functions return litmus objects.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{Main Plotting Functions}

\link{Functional Versions}\cr
Which take a function as their main argument.

\link{Global Options}

\link{mlitmus}, \link{litmus.fit}

\link{Predefined Litmus Objects}, \link{Predefined Litmus-Fitting Functions}\cr
With predefined colors.
}
\examples{
rainbow.litmus.3 <- function (a=0, b=1)
{   colvs <- cbind (c (110, 170, 230, 290), 42.5, 75)
    litmus (a, b, colvs, color.space="HCL")
}

colf <- rainbow.litmus.3 (-1, 1)

plot (colf)

colf (seq (-1, 1,, 10) )
}
