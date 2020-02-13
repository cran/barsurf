\name{07_barface_objects}
\alias{barface}
\alias{barface.heat}
\alias{barface.gold}
\alias{barface.blue}
\alias{barface.green}
\alias{barface.purple}
\title{Barface Objects}
\description{Color functions for 3d bar plots.}
\details{
A barface object is a function that maps a logical vector to a character vector, representing R colors.

The coltv defines a color, for true, which plot_bar uses for the tops of bars. The colfv defines a color, for false, which plot_bar uses for the sides of bars. It's possible to omit the colfv argument, in which case, the resulting color will be similar to the top color, but lighter.

The other functions (e.g. barface.heat) are wrappers, that create barface objects with particular colors.
}
\usage{
barface (coltv, colfv, color.space="sRGB")

barface.heat ()
barface.gold ()
barface.blue ()
barface.green ()
barface.purple ()
}
\arguments{
\item{coltv, colfv}{Length-3 or length-4 color vectors.}
\item{color.space}{A color space, refer to the details section for litmus objects.}
}
\value{
All these functions return barface objects.
}
\examples{
barface (c (0, 0, 0) )
}
\references{Refer to the vignette for an overview, references and better examples.}

