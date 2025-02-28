## ----app, eval=FALSE----------------------------------------------------------
# library(teal.modules.clinical)
# library(nestcolor)
# 
# ADSL <- tmc_ex_adsl
# ADAE <- tmc_ex_adae
# 
# app <- init(
#   data = cdisc_data(
#     ADSL = ADSL,
#     ADAE = ADAE,
#     code = "
#       ADSL <- tmc_ex_adsl
#       ADAE <- tmc_ex_adae
#     "
#   ),
#   modules = list(
#     tm_g_barchart_simple(
#       label = "ADAE Analysis",
#       x = data_extract_spec(
#         dataname = "ADAE",
#         select = select_spec(
#           choices = variable_choices(
#             ADAE,
#             c(
#               "ARM", "ACTARM", "SEX",
#               "RACE", "SAFFL", "STRATA2"
#             )
#           ),
#           selected = "ACTARM",
#           multiple = FALSE
#         )
#       )
#     )
#   )
# )
# 
# shinyApp(app$ui, app$server)

## ----shinylive_url, echo = FALSE, results = 'asis', eval = requireNamespace("roxy.shinylive", quietly = TRUE)----
code <- paste0(c(
  knitr::knit_code$get("app"),
  knitr::knit_code$get("shinyapp")
), collapse = "\n")

url <- roxy.shinylive::create_shinylive_url(code)
cat(sprintf("[Open in Shinylive](%s)\n\n", url))

## ----shinylive_iframe, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# knitr::include_url(url, height = "800px")

