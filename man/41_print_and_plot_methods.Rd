\name{41_supporting_methods}
\alias{print.litmus}
\alias{print.mlitmus}
\alias{plot.litmus}
\alias{plot.mlitmus}
\title{Supporting Methods}
\description{Print and plot methods for litmus and mlitmus objects.}
\usage{
\method{print}{litmus}(x, \dots)
\method{print}{mlitmus}(x, \dots)

\method{plot}{litmus}(x, n=200, \dots)
\method{plot}{mlitmus}(x, n=200, \dots)
}
\arguments{
\item{x}{A litmus or mlitmus object.}
\item{n}{Integer, number of strips.}
\item{\dots}{Ignored.}
}
\details{
The plot method calls colorspace::swatchplot, with a vector of colors.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{litmus}, \link{mlitmus}
}
\examples{
colf <- blue.litmus ()

print (colf)
plot (colf)
}
