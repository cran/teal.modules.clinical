% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_g_forest_rsp.R
\name{template_forest_rsp}
\alias{template_forest_rsp}
\title{Template: Response Forest Plot}
\usage{
template_forest_rsp(
  dataname = "ANL",
  parentname = "ADSL",
  arm_var,
  ref_arm = NULL,
  comp_arm = NULL,
  obj_var_name = "",
  aval_var = "AVALC",
  responders = c("CR", "PR"),
  subgroup_var,
  strata_var = NULL,
  stats = c("n_tot", "n", "n_rsp", "prop", "or", "ci"),
  riskdiff = NULL,
  conf_level = 0.95,
  col_symbol_size = NULL,
  font_size = 15,
  ggplot2_args = teal.widgets::ggplot2_args()
)
}
\arguments{
\item{dataname}{(\code{character})\cr analysis data used in teal module.}

\item{parentname}{(\code{character})\cr parent analysis data used in teal module, usually this refers to \code{ADSL}.}

\item{arm_var}{(\code{character})\cr variable names that can be used as \code{arm_var}.}

\item{ref_arm}{(\code{character})\cr the level of reference arm in case of arm comparison.}

\item{comp_arm}{(\code{character})\cr the level of comparison arm in case of arm comparison.}

\item{obj_var_name}{(\code{character})\cr additional text to append to the table title.}

\item{aval_var}{(\code{character})\cr name of the analysis value variable.}

\item{responders}{(\code{character})\cr values of \code{aval_var} that are considered to be responders.}

\item{subgroup_var}{(\code{character})\cr with variable names that can be used as subgroups.}

\item{strata_var}{(\code{character})\cr names of the variables for stratified analysis.}

\item{stats}{(\code{character})\cr the names of statistics to be reported among:
\itemize{
\item \code{n}: Total number of observations per group.
\item \code{n_rsp}: Number of responders per group.
\item \code{prop}: Proportion of responders.
\item \code{n_tot}: Total number of observations.
\item \code{or}: Odds ratio.
\item \code{ci} : Confidence interval of odds ratio.
\item \code{pval}: p-value of the effect.
Note, the statistics \code{n_tot}, \code{or}, and \code{ci} are required.
}}

\item{riskdiff}{(\code{list})\cr if a risk (proportion) difference column should be added, a list of settings to apply
within the column. See \code{\link[tern:control_riskdiff]{tern::control_riskdiff()}} for details. If \code{NULL}, no risk difference column will be added.}

\item{conf_level}{(\code{numeric})\cr value for the confidence level within the range of (0, 1).}

\item{col_symbol_size}{(\code{integer} or \code{NULL})\cr column index to be used to determine relative size for
estimator plot symbol. Typically, the symbol size is proportional to the sample size used
to calculate the estimator. If \code{NULL}, the same symbol size is used for all subgroups.}

\item{font_size}{(\code{numeric(1)})\cr font size.}

\item{ggplot2_args}{(\code{ggplot2_args}) optional\cr
object created by \code{\link[teal.widgets:ggplot2_args]{teal.widgets::ggplot2_args()}} with settings for the module plot. For this
module, this argument will only accept \code{ggplot2_args} object with \code{labs} list of following child
elements: \code{title}, \code{caption}. No other elements would be taken into account. The argument is
merged with option \code{teal.ggplot2_args} and with default module arguments (hard coded in the module body).

For more details, see the vignette: \code{vignette("custom-ggplot2-arguments", package = "teal.widgets")}.}
}
\value{
a \code{list} of expressions to generate a table or plot object.
}
\description{
Creates a valid expression to generate a response forest plot.
}
\seealso{
\code{\link[=tm_g_forest_rsp]{tm_g_forest_rsp()}}
}
\keyword{internal}
