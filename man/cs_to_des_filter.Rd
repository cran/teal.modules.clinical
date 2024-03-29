% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{cs_to_des_filter}
\alias{cs_to_des_filter}
\title{Convert choices_selected to data_extract_spec with only filter_spec}
\usage{
cs_to_des_filter(
  cs,
  dataname,
  multiple = FALSE,
  include_vars = FALSE,
  label = "Filter by"
)
}
\arguments{
\item{cs}{(\code{choices_selected})\cr object to be transformed. See \code{\link[teal.transform:choices_selected]{teal.transform::choices_selected()}} for details.}

\item{dataname}{(\code{character})\cr name of the data}

\item{multiple}{(\code{logical})\cr Whether multiple values shall be allowed in the
shiny \code{\link[shiny:selectInput]{shiny::selectInput()}}.}

\item{include_vars}{(\code{flag})\cr whether to include the filter variables as fixed selection
in the result. This can be useful for preserving for reuse in \code{rtables} code e.g.}

\item{label}{(\code{character})\cr Label to print over the selection field. For no label, set to \code{NULL}.}
}
\value{
(\code{\link[teal.transform:data_extract_spec]{teal.transform::data_extract_spec()}})
}
\description{
Convert choices_selected to data_extract_spec with only filter_spec
}
