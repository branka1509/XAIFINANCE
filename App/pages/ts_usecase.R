tsUsecaseUi <- function(id, label = "loan"){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width = 12,
            column(width = 12, includeHTML("./www/usecase.html")))
        
      )
    )
  )
}
