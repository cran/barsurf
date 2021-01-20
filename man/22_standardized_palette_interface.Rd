\name{22_standardized_palette_interface}
\alias{Standardized Palette Interface}
\alias{st.top.color}
\alias{st.side.color}
\alias{st.sgrid.color}
\alias{st.iso.colors}
\alias{st.litmus.fit}
\alias{st.litmus.fit.hcv}
\alias{st.litmus.fit.flow}
\alias{st.litmus.fit.lum}
\title{Standardized Palette Interface}
\description{Functions to get the default and global color objects.}
\usage{
st.top.color (theme)
st.side.color (theme)
st.sgrid.color (theme)
st.iso.colors (theme)

st.litmus.fit (theme)
st.litmus.fit.hcv (theme)
st.litmus.fit.flow (theme)
st.litmus.fit.lum (theme)
}
\arguments{
\item{theme}{Optional string, the name of a supported theme.\cr\cr
    Refer to \link{set.bs.theme}.}
}
\details{
Theses functions return strings or litmus-fitting functions objects.

If theme is provided, they return the strings or litmus-fitting functions, for a particular theme.\cr
If theme isn't provided, they return the strings or litmus-fitting functions, set by global options.
}
\value{
The top, side and sgrid functions, return a single (R color) string.\cr
The iso functions, return a character (R color) vector.

The litmus.fit functions, return a litmus-fitting function.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{set.bs.theme}, \link{set.bs.options}
}
\examples{
st.litmus.fit ("blue")
}
