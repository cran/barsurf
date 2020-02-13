\name{10_supporting_methods}
\alias{print.barface}
\alias{print.litmus}
\alias{print.mlitmus}
\alias{plot.barface}
\alias{plot.litmus}
\alias{plot.mlitmus}
\title{Supporting Methods}
\description{Print and plot methods for barface, litmus and mlitmus objects.}
\details{
The plot method calls colorspace::swatchplot, with a vector of colors.
}
\usage{
\method{print}{barface}(x, \dots)
\method{print}{litmus}(x, \dots)
\method{print}{mlitmus}(x, \dots)

\method{plot}{barface}(x, \dots)
\method{plot}{litmus}(x, n=200, \dots)
\method{plot}{mlitmus}(x, n=200, \dots)
}
\arguments{
\item{x}{A barface, litmus or mlitmus object.}
\item{n}{Number of strips.}
\item{\dots}{Ignored.}
}
\examples{
plot (barface.heat () )
}
