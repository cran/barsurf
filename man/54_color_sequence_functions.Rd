\name{54_color_sequence_functions}
\alias{blue.seq}
\alias{green.seq}
\alias{rainbow.seq}
\alias{heat.seq}
\alias{blue.fit}
\alias{green.fit}
\alias{rainbow.fit}
\alias{heat.fit}
\title{Color Sequences and Color Fitting Functions}
\description{
These are convenience functions that combine creating a litmus object and evaluating it, into one step.}
\usage{
blue.seq (n, \dots, hcv=FALSE)
green.seq (n, \dots, hcv=FALSE)
rainbow.seq (n, \dots)
heat.seq (n, \dots)

blue.fit (x, \dots, hcv=FALSE)
green.fit (x, \dots, hcv=FALSE)
rainbow.fit (x, \dots)
heat.fit (x, \dots)
}
\arguments{
\item{n}{Integer, the number of points, in the sequence.}
\item{x}{Numeric vector.}
\item{hcv}{Logical, if true, use the high color variation option.}
\item{\dots}{Ignored.}
}
\details{
The .seq functions construct a litmus object over the interval one to n, and then evaluate it with one to n.

The .fit function fit a litmus object to x, and then evaluate it with x.

Note that rainbow.seq is similar to colorspace::rainbow_hcl.
}
\value{A character vector of R colors.}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{litmus}, \link{litmus.fit}

\link{Predefined Litmus Objects}, \link{Predefined Litmus-Fitting Functions}
}
\examples{
blue.seq (10)
blue.fit (rnorm (30) )
}
