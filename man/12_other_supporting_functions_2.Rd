\name{12_other_supporting_functions_2}
\alias{matrix.margins}
\title{Matrix Margins}
\description{Reverse the bottom and top margins.}
\details{
The function changes the par settings for the margins, for subsequent plots.

It reverses the bottom and top margins, based on the assumption that the user wants the x-axis ticks/labels on the top, and a possible main title on the bottom.
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
plot_matrix (,,fv)
par (p0)
}
