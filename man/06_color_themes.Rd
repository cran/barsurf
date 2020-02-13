\name{06_color_themes}
\alias{set.bs.options}
\alias{set.bs.theme}
\title{Color Themes}
\description{Set global options and color themes.}
\usage{
set.bs.options (\dots, rendering.style="r", theme="blue")
set.bs.theme (theme)
}
\arguments{
\item{theme}{String, either "heat", "gold", "blue", "green" or "purple".}
\item{rendering.style}{Single character, either "r", "p" or "e", refer to details.}
\item{\dots}{Ignored.}
}
\details{
The set.bs.options and set.bs.theme functions set global options which determine line width and default colors.

The heat theme is designed for high impact, whereas the blue and green themes are designed for (higher) perceptual uniformity.

Currently, parts of the heat, gold and purple themes are the same as the blue theme.

By default, with the "r" rendering style (for standard R graphic devices), all lines have a line width of one and are black, with the exception of grid lines, isosurface lines and some contour lines. The "p" rendering style (for black and white printed documents) is the same as "r", except that all lines are black, with the exception of some contour lines. The "e" rendering style (for PDF and other documents, intended to be viewed electronically) is the same as "r", except that vector arrows, isosurfaces and grids use narrower lines.
}
\examples{
set.bs.theme ("heat")
}
