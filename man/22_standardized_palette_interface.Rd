\name{22_standardized_palette_interface}
\alias{st.top.color}
\alias{st.side.color}
\alias{st.sgrid.color}
\alias{st.iso.colors}
\alias{st.litmus.fit}
\alias{st.litmus.fit.hcv}
\alias{st.litmus.fit.flow}
\alias{st.litmus.fit.lum}
\alias{opt.top.color}
\alias{opt.side.color}
\alias{opt.sgrid.color}
\alias{opt.iso.colors}
\alias{opt.litmus.fit}
\alias{opt.litmus.fit.hcv}
\alias{opt.litmus.fit.glass}
\alias{opt.litmus.fit.flow}
\alias{opt.litmus.fit.lum}
\title{Standardized Palette Interface}
\description{Functions to get the default color objects.}
\usage{
st.top.color (theme)
st.side.color (theme)
st.sgrid.color (theme)
st.iso.colors (theme)

st.litmus.fit (theme)
st.litmus.fit.hcv (theme)
st.litmus.fit.flow (theme)
st.litmus.fit.lum (theme)

opt.top.color ()
opt.side.color ()
opt.sgrid.color ()
opt.iso.colors ()

opt.litmus.fit ()
opt.litmus.fit.hcv ()
opt.litmus.fit.glass ()
opt.litmus.fit.flow ()
opt.litmus.fit.lum ()
}
\arguments{
\item{theme}{String, the name of a supported theme.}
}
\details{
The st functions, return objects, for a particular theme.

And the opt functions, return objects, set by global options.
}
\value{
The top, side and sgrid functions, return a single (R color) string.\cr
The iso functions, return a character (R color) vector.

And the litmus.fit functions, return a litmus-fitting function.
}
\references{Refer to the vignette for an overview, references and better examples.}
\seealso{
\link{set.bs.options}\cr
This provides information about themes.
}
\examples{
st.litmus.fit ("blue")
}
