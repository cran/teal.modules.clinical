% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_t_pp_prior_medication.R
\name{template_prior_medication}
\alias{template_prior_medication}
\title{Template: Patient Profile Prior Medication}
\usage{
template_prior_medication(
  dataname = "ANL",
  atirel = "ATIREL",
  cmdecod = "CMDECOD",
  cmindc = "CMINDC",
  cmstdy = "CMSTDY"
)
}
\arguments{
\item{dataname}{(\code{character})\cr analysis data used in teal module.}

\item{atirel}{(\code{character})\cr name of time relation of medication variable.}

\item{cmdecod}{(\code{character})\cr name of standardized medication name variable.}

\item{cmindc}{(\code{character})\cr name of indication variable.}

\item{cmstdy}{(\code{character})\cr name of study relative day of start of medication variable.}
}
\value{
a \code{list} of expressions to generate a table or plot object.
}
\description{
Creates a valid expression to generate a patient profile prior medication report using ADaM datasets.
}
\seealso{
\code{\link[=tm_t_pp_prior_medication]{tm_t_pp_prior_medication()}}
}
\keyword{internal}
