\name{21_global_options}
\alias{Global Options}
\alias{set.bs.options}
\alias{set.bs.nhl}
\alias{set.bs.theme}
\alias{opt.nhl}
\alias{opt.ref.arrows}
\title{Global Options}
\description{Set/get global options.}
\usage{
set.bs.options (\dots,
    nhl=1, ref.arrows=TRUE,
    rendering.style="R", theme="blue",
    top.color, side.color, sgrid.color, iso.colors,
    main, hcv, glass="glass.rainbow.fit", flow, lum,
    test.mode=FALSE)

set.bs.nhl (nhl)
set.bs.theme (theme)

opt.nhl ()
opt.ref.arrows ()
}
\arguments{
\item{nhl}{Integer, the number of header (main title) lines, in 3d plots.}
\item{ref.arrows}{Logical vector of length one or two, if true plot reference arrows.}
\item{rendering.style}{Single string, either "R" or "pdf", refer to details.}
\item{theme}{String, either "gold", "blue", "green", "purple" or "heat".}
\item{top.color, side.color}{Strings (R colors), the bar colors.}
\item{sgrid.color}{String (R color), the surface grid color.}
\item{iso.colors}{Character vector (R colors), the initial isosurface colors.}
\item{main}{String, name of the main litmus-fitting function, which is the default for heatmaps, with hcv=FALSE.}
\item{hcv}{Sting, name of the hcv (high color variation) litmus-fitting function, which is the default for heatmaps, with hcv=TRUE.}
\item{glass}{String, name of the glass litmus-fitting function, which is the default for plot_cfield3.}
\item{flow}{String, name of the flow litmus-fitting function, which is the default for vector fields.}
\item{lum}{String, name of the lum litmus-fitting function, which is the default for surface plots.}
\item{test.mode}{If true, and the main plotting functions are called with unmatched arguments, then an error is generated.\cr\cr
	This should be false, except for testing purposes.}
\item{\dots}{Ignored.}
}
\details{
The set functions, set global options, and the opt functions gets them.

The "R" rendering style is for standard R graphic devices, and raster images.\cr
The "pdf" rendering style is for PDF documents and other scalable vector formats.

The "pdf" option is the same as the "R" option, except that vector arrow lines, grid lines and wire frame lines are narrower.\cr
However, the end result is dependent on the PDF viewer.

The blue and green themes are designed for relatively high perceptual uniformity.\cr
And the heat theme is designed for high impact.

Calling the set.bs.options function will set all the options.\cr
The default values of most color-based arguments are dependent on the theme.

In contrast, calling the set.bs.nhl functions will only change the nhl option, and calling the set.bs.theme function, will only change color-related options.
}
\note{
Do not change global options, directly.

But rather, use set.bs.options, set.nhl or set.theme functions.
}
\references{Refer to the vignette for an overview, references and better examples.}
\examples{
set.bs.options (nhl=2, ref.arrows=FALSE)
set.bs.theme ("gold")
}
