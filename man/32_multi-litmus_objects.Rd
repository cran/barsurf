\name{32_multi-litmus_objects}
\alias{mlitmus}
\title{Multi-Litmus Objects}
\description{Color functions that combine multiple litmus objects.}
\usage{
mlitmus (\dots, default.color="#D0D0D0", na.color=default.color)
}
\arguments{
\item{\dots}{One or more litmus objects.}
\item{default.color}{String, color that is returned for values outside the litmus objects' knots.}
\item{na.color}{String, color that is returned for NA values.}
}
\details{
This function creates a color function similar to a litmus object, containing one or more litmus objects.\cr
(Noting that these functions return mlitmus objects, so you call these functions, and then if necessary you can evaluate the resulting function).

The color function works out which litmus object to use for each input value.

An example is using one set of colors for positive values and another set of colors for negative values.

Note that it's possible for litmus objects to overlap.\cr
This may be changed, so this feature should not be used inside packages.
}
\value{
An mlitmus object.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{litmus}, \link{litmus.fit}

\link{Predefined Litmus Objects}, \link{Predefined Litmus-Fitting Functions}\cr
With predefined colors.

\link{hot.and.cold}
}
\examples{
colf <- mlitmus (blue.litmus (0, -1), green.litmus (0, 1) )

plot (colf)
colf (c (-1, 0, 1) )
}
