loanUi <- function(id, label = "loan"){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width = 12, title = "Data description & pre-procesing",
            column(width = 12, withMathJax(includeMarkdown("./www/loan.Rmd"))))
    
        )
    )
  )
}
