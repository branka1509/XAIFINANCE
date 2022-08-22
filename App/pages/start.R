print("Loading Start page")
startUi <- function(id){
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width = 12, title = "Project Description",
        column(width = 12, withMathJax(includeMarkdown("./www/start_page.Rmd"))))
      )
    )
  )
}

