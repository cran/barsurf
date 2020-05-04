\name{07_barface_objects}
\alias{barface}
\alias{heat.barface}
\alias{gold.barface}
\alias{blue.barface}
\alias{green.barface}
\alias{purple.barface}
\title{Barface Objects}
\description{Color functions for 3d bar plots.}
\details{
A barface object is a function that maps a logical vector to a character vector, representing R colors.\cr
(Noting that these functions return barface objects, so you call these functions, and then if necessary you can evaluate the resulting function).

The coltv defines a color, for true, which plot_bar uses for the tops of bars. The colfv defines a color, for false, which plot_bar uses for the sides of bars. It's possible to omit the colfv argument, in which case, the resulting color will be similar to the top color, but lighter.

The other functions (e.g. heat.barface) are wrappers, that create barface objects with particular colors.
}
\usage{
barface (coltv, colfv, \dots, color.space="sRGB")

heat.barface ()
gold.barface ()
blue.barface ()
green.barface ()
purple.barface ()
}
\arguments{
\item{coltv, colfv}{Length-3 or length-4 numeric vectors, representing colors.}
\item{color.space}{String, a color space, refer to the details section for litmus objects.}
\item{\dots}{Ignored.}
}
\value{
All these functions return barface objects.

Refer to details.
}
\examples{
colf <- barface (c (1, 0, 0), c (0, 0, 1) )
plot (colf)

#evaluate
colf (c (TRUE, FALSE) )
}
\references{Refer to the vignette for an overview, references and better examples.}

\seealso{\link{litmus} }

