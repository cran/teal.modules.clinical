% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{color_lab_values}
\alias{color_lab_values}
\title{Mapping function for Laboratory Table}
\usage{
color_lab_values(
  x,
  classes = c("HIGH", "NORMAL", "LOW"),
  colors = list(HIGH = "red", NORMAL = "grey", LOW = "blue"),
  default_color = "black",
  icons = list(HIGH = "glyphicon glyphicon-arrow-up", LOW =
    "glyphicon glyphicon-arrow-down")
)
}
\arguments{
\item{x}{(\code{character})\cr vector with elements under the format (\verb{value level}).}

\item{classes}{(\code{character})\cr classes vector.}

\item{colors}{(\code{list})\cr color per class.}

\item{default_color}{(\code{character})\cr default color.}

\item{icons}{(\code{list})\cr certain icons per level.}
}
\value{
a character vector where each element is a formatted HTML tag corresponding to
a value in \code{x}.
}
\description{
Map value and level characters to values with with proper html tags, colors and icons.
}
\examples{
color_lab_values(c("LOW", "LOW", "HIGH", "NORMAL", "HIGH"))

}
