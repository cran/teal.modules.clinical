# This file contains functions that help with plotting in other modules

#' Facetting formula `x_facet ~ y_facet`
#'
#' @description `r lifecycle::badge("stable")`
#' Replaces `x_facet` or `y_facet` by . when empty character
#'
#' @md
#' @param x_facet (`character(1)`)\cr
#'  name of x facet, if empty, will not facet along x.
#' @param y_facet (`character(1)`)\cr
#'  name of y facet, if empty, will not facet along y.
#'
#' @return facet grid formula `formula(x_facet ~ y_facet)`
#'
#' @keywords internal
#'
facet_grid_formula <- function(x_facet, y_facet) {
  if (length(x_facet) == 0) x_facet <- "."
  if (length(y_facet) == 0) y_facet <- "."
  checkmate::assert_string(x_facet)
  checkmate::assert_string(y_facet)
  if (x_facet == y_facet) stop("'x_facet' and 'y_facet' must not be equal.")
  stats::as.formula(paste0(y_facet, " ~ ", x_facet)) # must invert it
}
