\name{53_predefined_litmus-fitting_functions}
\alias{Predefined Litmus-Fitting Functions}
\alias{gold.litmus.fit}
\alias{blue.litmus.fit}
\alias{green.litmus.fit}
\alias{heat.litmus.fit}
\alias{blue.litmus.fit.hcv}
\alias{green.litmus.fit.hcv}
\alias{blue.litmus.fit.flow}
\alias{green.litmus.fit.flow}
\alias{gold.litmus.fit.lum}
\alias{blue.litmus.fit.lum}
\alias{green.litmus.fit.lum}
\alias{purple.litmus.fit.lum}
\alias{heat.litmus.fit.lum}
\alias{rainbow.litmus.fit}
\alias{rainbow.litmus.fit.2}
\alias{glass.rainbow.fit}
\alias{hot.and.cold.fit}
\title{Predefined Litmus-Fitting Functions}
\description{Functions that wrap the main litmus-fitting function, with predefined colors.}
\usage{
gold.litmus.fit (x, \dots, reverse=FALSE, equalize=0.85)
blue.litmus.fit (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit (x, \dots, reverse=FALSE, equalize=0.85)
heat.litmus.fit (x, \dots, reverse=FALSE, equalize=0.85)

blue.litmus.fit.hcv (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit.hcv (x, \dots, reverse=FALSE, equalize=0.85)
blue.litmus.fit.flow (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit.flow (x, \dots, reverse=FALSE, equalize=0.85)

gold.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
blue.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
green.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
purple.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)
heat.litmus.fit.lum (x, \dots, reverse=FALSE, equalize=0.85)

rainbow.litmus.fit (x, \dots, c=42.5, l=75, start=65, end=315,
    equalize=0.85)
rainbow.litmus.fit.2 (x, \dots, c=50, l=70, start=0, end=360,
    equalize=0.85)
glass.rainbow.fit (x, alpha=0.3, \dots, c=42.5, l=62.5, start=42.5, end=260,
    equalize=0.85)

hot.and.cold.fit (x, \dots, t=0, symetric=TRUE, hot.hue=35, cold.hue=255,
    equalize=0.85)
}
\arguments{
\item{x}{A numeric vector.}
\item{reverse}{Logical, reverse the order of the colors.}
\item{equalize}{Numeric, between zero and one, refer to details, for litmus.fit.}
\item{c, l, start, end}{Same as colorspace::rainbow_hcl.}
\item{alpha}{A numeric vector giving the alpha component. If it has two or more values, then each alpha value is assigned to each knot.}
\item{t}{Numeric of length one or two, giving the transition points between "hot" and cold".\cr Two points can be used for a softer effect, but they should be relatively close.}
\item{symetric}{Logical, if true, the hot and cold sides should be relatively symetrical.}
\item{hot.hue, cold.hue}{Numerics, the hot and cold hues.}
\item{\dots}{Ignored.}
}
\details{
Most of these functions wrap the litmus.fit function.

The rainbow.litmus.fit and rainbow.litmus.fit.2 functions are based on colorspace::rainbow_hcl.
}
\value{
All functions return litmus objects.\cr
(Except for hot.and.cold.fit, which returns a mlitmus object).
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{Main Plotting Functions}

\link{Functional Versions}\cr
Which take a function as their main argument.

\link{Global Options}

\link{litmus}, \link{mlitmus}, \link{litmus.fit}

\link{Predefined Litmus Objects}
}
\examples{
x <- rnorm (30)

colf1 <- blue.litmus.fit (x)
colf2 <- hot.and.cold.fit (x)

p0 <- par (mfrow = c (2, 1) )
plot (colf1)
plot (colf2)
par (p0)

u <- seq (min (x), max (x),, 5)
colf1 (u)
colf2 (u)
}
