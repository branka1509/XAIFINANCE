xfunctionUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width = 12,
            column(width = 12, withMathJax(includeMarkdown("./www/x_function.Rmd"))))# includeHTML("./www/x_function.html")))
      )
    )
  )
}

