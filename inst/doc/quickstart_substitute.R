## -----------------------------------------------------------------------------
non_evaluated_expression <- substitute(expr = a + b)
a <- 1
b <- 5
eval(non_evaluated_expression)

## -----------------------------------------------------------------------------
fun <- function(a, b) {
  substitute(expr = a + b)
}
non_evaluated_expression <- fun(5, -2)
non_evaluated_expression
eval(non_evaluated_expression)

## -----------------------------------------------------------------------------
non_evaluated_expression <- substitute(
  expr = a + b,
  env = list(a = 5, b = 5)
)
non_evaluated_expression
eval(non_evaluated_expression)

## -----------------------------------------------------------------------------
non_evaluated_expression <- substitute(
  expr = plot(x = x, y = exp(x), main = text),
  env = list(x = 0:10, text = "A graph")
)
non_evaluated_expression
eval(non_evaluated_expression)

## -----------------------------------------------------------------------------
plot_expr <- substitute(
  expr = plot(y ~ x, data = iris, main = text),
  env = list(
    x = as.name("Sepal.Length"),
    y = as.symbol("Sepal.Width"),
    text = "Iris, again ..."
  )
)
plot_expr
eval(plot_expr)

## -----------------------------------------------------------------------------
library(dplyr)

short_iris <- head(iris)
plot_expr <- substitute(
  expr = df %>% plot(y ~ x, data = ., main = text),
  env = list(
    df = short_iris,
    x = as.name("Sepal.Length"),
    y = as.symbol("Sepal.Width"),
    text = "Iris, again ..."
  )
)
eval(plot_expr)
plot_expr

## -----------------------------------------------------------------------------
plot_expr <- substitute(
  expr = df %>% plot(y ~ x, data = ., main = text),
  env = list(
    df = substitute(iris),
    x = as.name("Sepal.Length"),
    y = as.symbol("Sepal.Width"),
    text = "Iris, again ..."
  )
)
plot_expr
eval(plot_expr)

## ----message=FALSE------------------------------------------------------------
library(teal.modules.clinical)
library(rtables)
library(tern)
library(dplyr)

adlb <- tmc_ex_adlb
adlb_f <- adlb %>%
  filter(
    PARAM == "Alanine Aminotransferase Measurement" &
      ARMCD %in% c("ARM A", "ARM B") & AVISIT == "WEEK 1 DAY 8"
  )

## -----------------------------------------------------------------------------
rtables_expr <- substitute(
  expr = basic_table() %>%
    split_cols_by(arm, split_fun = drop_split_levels) %>%
    split_rows_by(visit, split_fun = drop_split_levels) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Value", "Change")
    ) %>%
    summarize_colvars() %>%
    build_table(df = df),
  env = list(
    df = substitute(adlb_f),
    arm = "ARM",
    visit = "AVISIT"
  )
)

## -----------------------------------------------------------------------------
eval(rtables_expr)

## -----------------------------------------------------------------------------
rtables_expr

## ----message = FALSE----------------------------------------------------------
library(teal)
library(styler)

#' Stylish code
#'
#' Deparse an expression and display the code following NEST conventions.
#'
#' @param expr (`call`)\cr or possibly understood as so.
#'
styled_expr <- function(expr) {
  print(
    styler::style_text(text = deparse(expr)),
    colored = FALSE
  )
}
#'
#' @examples
styled_expr(rtables_expr)

## -----------------------------------------------------------------------------
rtables_expr <- function(df,
                         arm,
                         visit) {
  substitute(
    expr = basic_table() %>%
      split_cols_by(arm, split_fun = drop_split_levels) %>%
      split_rows_by(visit, split_fun = drop_split_levels) %>%
      split_cols_by_multivar(
        vars = c("AVAL", "CHG"),
        varlabels = c("Value", "Change")
      ) %>%
      summarize_colvars() %>%
      build_table(df = df),
    env = list(
      df = substitute(df),
      arm = arm,
      visit = visit
    )
  )
}
result <- rtables_expr(df = adlb_f, arm = "ARM", visit = "AVISIT")
styled_expr(result)
eval(result)

## -----------------------------------------------------------------------------
result <- rtables_expr(df = adlb_f, arm = "ARMCD", visit = "AVISITN")
eval(result)
styled_expr(result)

## -----------------------------------------------------------------------------
#' Expressions as a pipeline
#'
#' Accepts expressions to be chained using the `magrittr` pipeline-flavor.
#' @param ... (`call`)\cr or object which can be interpreted as so.
#'    (e.g. `name`)
#'
pipe_expr <- function(...) {
  exprs <- unlist(list(...))
  exprs <- lapply(
    exprs,
    function(x) {
      x <- deparse(x)
      paste(x, collapse = " ")
    }
  )
  exprs <- unlist(exprs)
  exprs <- paste(exprs, collapse = " %>% ")
  str2lang(exprs)
}

#' @examples
result <- pipe_expr(
  expr1 = substitute(df),
  expr2 = substitute(head)
)
result

## -----------------------------------------------------------------------------
rtables_expr <- function(df,
                         arm,
                         visit,
                         .stats = NULL) {
  # The rtables layout is decomposed into a list of expressions.
  lyt <- list()
  # 1. First the columns and rows:
  lyt$structure <- substitute(
    expr = basic_table() %>%
      split_cols_by(arm, split_fun = drop_split_levels) %>%
      split_rows_by(visit, split_fun = drop_split_levels) %>%
      split_cols_by_multivar(
        vars = c("AVAL", "CHG"),
        varlabels = c("Value", "Change")
      ),
    env = list(
      arm = arm,
      visit = visit
    )
  )
  # 2. The analyze layer which depends on the use of .stats.
  lyt$analyze <- if (is.null(.stats)) {
    substitute(
      summarize_colvars()
    )
  } else {
    substitute(
      summarize_colvars(.stats = .stats),
      list(.stats = .stats)
    )
  }
  # 3. And finishing with rtables::build_table.
  lyt$build <- substitute(
    build_table(df = df),
    list(df = substitute(df))
  )
  # As previously demonstrated, expressions can be manipulated and
  # chained in a pipeline.
  pipe_expr(lyt)
}

## -----------------------------------------------------------------------------
result <- rtables_expr(df = adlb_f, arm = "ARM", visit = "AVISIT")
styled_expr(result)
eval(result)

## -----------------------------------------------------------------------------
result <- rtables_expr(
  df = adlb_f, arm = "ARM", visit = "AVISIT",
  .stats = c("n", "mean_sd")
)
styled_expr(result)
eval(result)

## -----------------------------------------------------------------------------
rtables_expr <- function(df,
                         paramcd,
                         arm,
                         visit,
                         .stats = NULL) {
  # y is a list which will collect two expressions:
  # 1. y$data with the preprocessing steps.
  # 2. y$rtables the table layout and build.
  y <- list()
  # 1. Preprocessing ---
  y$data <- substitute(
    df <- df %>%
      filter(
        PARAMCD == paramcd &
          ARMCD %in% c("ARM A", "ARM B") & AVISIT == "WEEK 1 DAY 8"
      ),
    list(
      df = substitute(df),
      paramcd = paramcd
    )
  )
  # 2. rtables layout ---
  lyt <- list()
  lyt$structure <- substitute(
    expr = basic_table() %>%
      split_cols_by(arm, split_fun = drop_split_levels) %>%
      split_rows_by(visit, split_fun = drop_split_levels) %>%
      split_cols_by_multivar(
        vars = c("AVAL", "CHG"),
        varlabels = c("Value", "Change")
      ),
    env = list(
      arm = arm,
      visit = visit
    )
  )
  lyt$analyze <- if (is.null(.stats)) {
    substitute(
      summarize_colvars()
    )
  } else {
    substitute(
      summarize_colvars(.stats = .stats),
      list(.stats = .stats)
    )
  }
  lyt$build <- substitute(
    build_table(df = df),
    list(df = substitute(df))
  )
  y$rtables <- pipe_expr(lyt)
  # Finally returns y as a list with two expressions.
  y
}

## -----------------------------------------------------------------------------
adlb <- tmc_ex_adlb
result <- rtables_expr(
  df = adlb, paramcd = "CRP", arm = "ARM", visit = "AVISIT",
  .stats = c("n", "mean_sd")
)

## -----------------------------------------------------------------------------
styled_expr(result$data)
styled_expr(result$rtables)

## -----------------------------------------------------------------------------
result_exec <- mapply(eval, result)
result_exec$rtables

