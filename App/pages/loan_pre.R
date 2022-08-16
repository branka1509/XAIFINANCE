loanPreUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Preprocessing"))),
      fluidRow(
        box(width = 12, h3("Here we describe the steps done."))
      )
      
    )
  )
}

loanPreServer  <- function(id){
  moduleServer(
    id,
    function(input, output, session){
    }
  )
}