\name{52_predefined_litmus_objects}
\alias{Predefined Litmus Objects}
\alias{blue.litmus}
\alias{green.litmus}
\alias{heat.litmus}
\alias{blue.litmus.hcv}
\alias{green.litmus.hcv}
\alias{blue.litmus.flow}
\alias{green.litmus.flow}
\alias{rainbow.litmus}
\alias{rainbow.litmus.2}
\alias{glass.litmus}
\alias{hot.and.cold}
\title{Predefined Litmus Objects}
\description{Functions that wrap the main litmus constructor, with predefined colors.}
\usage{
blue.litmus (a=0, b=1)
green.litmus (a=0, b=1)
heat.litmus (a=0, b=1)

blue.litmus.hcv (a=0, b=1)
green.litmus.hcv (a=0, b=1)
blue.litmus.flow (a=0, b=1)
green.litmus.flow (a=0, b=1)

rainbow.litmus (a=0, b=1, \dots, c=42.5, l=75, start=65, end=315)
rainbow.litmus.2 (a=0, b=1, \dots, c=50, l=70, start=0, end=360)
glass.litmus (a=0, b=1, alpha=0.3, \dots, c=42.5, l=62.5, start=42.5, end=260)

hot.and.cold (t=0, d=1, \dots, hot.hue=35, cold.hue=255)
}
\arguments{
\item{a, b}{Numerics, the lower and upper limits.}
\item{c, l, start, end}{Same as colorspace::rainbow_hcl.}
\item{alpha}{A numeric vector giving the alpha component. If it has two or more values, then each alpha value is assigned to each color.}
\item{t}{Numeric of length one or two, giving the transition points between "hot" and cold".\cr Two points can be used for a softer effect, but they should be relatively close.}
\item{d}{Numeric of length one or two, giving the distance about the transition point.}
\item{hot.hue, cold.hue}{Numerics, the hot and cold hues.}
\item{\dots}{Ignored.}
}
\details{
Most of these functions wrap the litmus function.

The rainbow.litmus and rainbow.litmus.2 functions are based on colorspace::rainbow_hcl.
}
\value{
All functions return litmus objects.\cr
(Except for hot.and.cold, which returns a mlitmus object).
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{Main Plotting Functions}

\link{Functional Versions}\cr
Which take a function as their main argument.

\link{Global Options}

\link{litmus},\link{mlitmus}, \link{litmus.fit}

\link{Predefined Litmus-Fitting Functions}
}
\examples{
colf1 <- blue.litmus (-1, 1)
colf2 <- hot.and.cold ()

p0 <- par (mfrow = c (2, 1) )
plot (colf1)
plot (colf2)
par (p0)

u <- seq (-1, 1,, 5)
colf1 (u)
colf2 (u)
}
