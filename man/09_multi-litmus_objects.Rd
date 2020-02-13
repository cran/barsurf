\name{09_multi-litmus_objects}
\alias{mlitmus}
\alias{hot.and.cold}
\title{Multi-Litmus Objects}
\description{Color functions that combine multiple litmus objects.}
\details{
This function creates a color function similar to a litmus object, containing one or more litmus objects.

The color function works out which litmus object to use for each input value.

An example is using one set of colors for positive values and another set of colors for negative values.

Note that it's possible for litmus objects to overlap.\cr
This may be changed, so this feature should not be used inside packages.
}
\usage{
mlitmus (\dots, default.col="#D0D0D0", na.col=default.col)

hot.and.cold (a=-1, b=1, xb=0)
}
\arguments{
\item{\dots}{One or more litmus objects.}
\item{default.col}{String, color that is returned for x values outside the litmus objects' knots.}
\item{na.col}{String, color that is returned for NA x values.}
\item{a}{The lower limit (first knot).}
\item{b}{The upper limit (last knot).}
\item{xb}{Numeric (scalar), giving the breakpoint between "hot" and cold".}
}
\value{
An mlitmus object.
}
\examples{
mlitmus (litmus.green (-1, 0, reverse=TRUE), litmus.blue (0, 1) )
}
\references{Refer to the vignette for an overview, references and better examples.}

