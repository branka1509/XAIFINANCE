timeUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width = 12, title = "Project Description",
            column(width = 12, includeHTML("./www/time.html")))
      )
    )
  )
}

