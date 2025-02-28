## ----setup, include=FALSE-----------------------------------------------------
library(teal.modules.clinical)

## ----decorate_listing_df, message=FALSE---------------------------------------
library(teal.modules.clinical)

data <- within(teal_data(), {
  library(dplyr)
  ADSL <- tmc_ex_adsl |>
    mutate(
      ITTFL = factor("Y") |> with_label("Intent-To-Treat Population Flag")
    ) |>
    mutate(DTHFL = case_when(!is.na(DTHDT) ~ "Y", TRUE ~ "") |> with_label("Subject Death Flag"))


  ADLB <- tmc_ex_adlb |>
    mutate(AVISIT == forcats::fct_reorder(AVISIT, AVISITN, min)) |>
    mutate(
      ONTRTFL = case_when(
        AVISIT %in% c("SCREENING", "BASELINE") ~ "",
        TRUE ~ "Y"
      ) |> with_label("On Treatment Record Flag")
    )
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

insert_rrow_decorator <- function(default_caption = "I am a good new row") {
  teal_transform_module(
    label = "New row",
    ui = function(id) {
      shiny::textInput(shiny::NS(id, "new_row"), "New row", value = default_caption)
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                table <- rtables::insert_rrow(table, rtables::rrow(new_row))
              },
              new_row = input$new_row
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_t_abnormality(
      label = "tm_t_abnormality",
      dataname = "ADLB",
      arm_var = choices_selected(
        choices = variable_choices("ADSL", subset = c("ARM", "ARMCD")),
        selected = "ARM"
      ),
      add_total = FALSE,
      by_vars = choices_selected(
        choices = variable_choices("ADLB", subset = c("LBCAT", "PARAM", "AVISIT")),
        selected = c("LBCAT", "PARAM"),
        keep_order = TRUE
      ),
      baseline_var = choices_selected(
        variable_choices("ADLB", subset = "BNRIND"),
        selected = "BNRIND", fixed = TRUE
      ),
      grade = choices_selected(
        choices = variable_choices("ADLB", subset = "ANRIND"),
        selected = "ANRIND",
        fixed = TRUE
      ),
      abnormal = list(low = "LOW", high = "HIGH"),
      exclude_base_abn = FALSE,
      decorators = list(table = insert_rrow_decorator("I am a good new row"))
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_1, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_listing_df")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----decorate_ggplot, message=FALSE-------------------------------------------
library(teal.modules.clinical)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  require(nestcolor)
  ADSL <- rADSL
  ADTTE <- tmc_ex_adtte
})
join_keys(data) <- default_cdisc_join_keys[names(data)]


ggplot_caption_decorator <- function(default_caption = "I am a good decorator") {
  teal_transform_module(
    label = "Caption",
    ui = function(id) {
      shiny::textInput(shiny::NS(id, "title"), "Plot Title", value = default_caption)
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |>
            within(
              {
                plot <- plot +
                  ggplot2::ggtitle(title) +
                  cowplot::theme_cowplot()
              },
              title = input$title
            )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_g_km(
      label = "tm_g_km",
      dataname = "ADTTE",
      arm_var = choices_selected(
        variable_choices("ADSL", c("ARM", "ARMCD", "ACTARMCD")),
        "ARM"
      ),
      paramcd = choices_selected(
        value_choices("ADTTE", "PARAMCD", "PARAM"),
        "OS"
      ),
      arm_ref_comp = list(
        ACTARMCD = list(ref = "ARM B", comp = c("ARM A", "ARM C")),
        ARM = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination"))
      ),
      strata_var = choices_selected(
        variable_choices("ADSL", c("SEX", "BMRKR2")),
        "SEX"
      ),
      facet_var = choices_selected(
        variable_choices("ADSL", c("SEX", "BMRKR2")),
        NULL
      ),
      decorators = list(plot = ggplot_caption_decorator())
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_2, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_ggplot")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

## ----decorate_datatable, message=FALSE----------------------------------------
library(teal.modules.clinical)

data <- teal_data(join_keys = default_cdisc_join_keys[c("ADSL", "ADRS")])
data <- within(data, {
  ADSL <- rADSL
  ADLB <- tmc_ex_adlb |>
    mutate(AVISIT == forcats::fct_reorder(AVISIT, AVISITN, min)) |>
    mutate(
      ONTRTFL = case_when(
        AVISIT %in% c("SCREENING", "BASELINE") ~ "",
        TRUE ~ "Y"
      ) |> with_label("On Treatment Record Flag")
    )
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

dt_table_decorator <- function(color1 = "pink", color2 = "lightblue") {
  teal_transform_module(
    label = "Table color",
    ui = function(id) {
      selectInput(
        NS(id, "color"),
        "Table Color",
        choices = c("white", color1, color2),
        selected = "Default"
      )
    },
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        reactive({
          data() |> within(
            {
              table <- DT::formatStyle(
                table,
                columns = attr(table$x, "colnames")[-1],
                target = "row",
                backgroundColor = color
              )
            },
            color = input$color
          )
        })
      })
    }
  )
}

app <- init(
  data = data,
  modules = modules(
    tm_t_pp_laboratory(
      label = "tm_t_pp_laboratory",
      dataname = "ADLB",
      patient_col = "USUBJID",
      paramcd = choices_selected(
        choices = variable_choices("ADLB", "PARAMCD"),
        selected = "PARAMCD"
      ),
      param = choices_selected(
        choices = variable_choices("ADLB", "PARAM"),
        selected = "PARAM"
      ),
      timepoints = choices_selected(
        choices = variable_choices("ADLB", "ADY"),
        selected = "ADY"
      ),
      anrind = choices_selected(
        choices = variable_choices("ADLB", "ANRIND"),
        selected = "ANRIND"
      ),
      aval_var = choices_selected(
        choices = variable_choices("ADLB", "AVAL"),
        selected = "AVAL"
      ),
      avalu_var = choices_selected(
        choices = variable_choices("ADLB", "AVALU"),
        selected = "AVALU"
      ),
      decorators = list(table = dt_table_decorator())
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

## ----shinylive_iframe_3, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# code <- paste0(c(
#   "interactive <- function() TRUE",
#   knitr::knit_code$get("setup"),
#   knitr::knit_code$get("decorate_datatable")
# ), collapse = "\n")
# 
# url <- roxy.shinylive::create_shinylive_url(code)
# knitr::include_url(url, height = "800px")

