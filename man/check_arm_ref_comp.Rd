% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arm_ref_comp.R
\name{check_arm_ref_comp}
\alias{check_arm_ref_comp}
\title{Check if the Treatment variable is reference or compare}
\usage{
check_arm_ref_comp(x, df_to_check, module)
}
\arguments{
\item{x}{(\code{character})\cr Name of the variable}

\item{df_to_check}{(\code{data.frame})\cr table to check}

\item{module}{(\code{character})\cr teal module the ref and comp are called in}
}
\value{
\code{TRUE} or \code{FALSE} whether the variable is in ref or comp
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\keyword{internal}
