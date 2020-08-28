\name{62_matrix_plot_margins}
\alias{matrix.margins}
\title{Matrix Margins}
\description{Reverse the top and bottom margins.}
\details{
The function changes the par settings for the margins, for subsequent plots.

It reverses the top and bottom margins, based on the assumption that the user wants the x-axis ticks/labels on the top, and a possible main title on the bottom.
}
\usage{
matrix.margins ()
}
\value{
A named list giving the original par settings for the margins.
}
\examples{
fv <- matrix (sample (1:24), 4, 6)

p0 <- matrix.margins ()
plot_matrix (,,fv, main="my plot")
par (p0)
}
