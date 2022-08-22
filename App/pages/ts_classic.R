tsClassicUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Time Series Classic XAI"))),
      fluidRow(
        box(width=12,withMathJax(includeMarkdown("www/classic_top.Rmd")))),
      fluidRow(
        column(6,
               box(
                 uiOutput(ns("method")),
                 width = 12)
        ),
        column(6,
               box(
                 plotOutput(ns("plot_method")),
                 width = 12)
        )
      ),
      fluidRow(
        box(width=12,withMathJax(includeMarkdown("www/classic_bottom.Rmd")))),
    )
  )
}

tsClassicServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      output$method <- renderUI({
        selectInput(
          ns("method"), 
          label = "Select the XAI method",
          choices = c(
            "Residual Analysis" = "residual_analysis",
            "Variable Importance" = "varimp",
            "PDP" = "pdp",
            "ICE" = "ice",
            "SHAP" = "shap")
        )
      })
      output$plot_method <- renderPlot({
        if (input$method == "pdp"){
          classic[["pdp"]]
        } else if (input$method == "ice"){
          classic[["ice"]]
        } else if (input$method == "residual_analysis"){
          classic[["residual_analysis"]]
        } else if (input$method == "varimp"){
          classic[["varimp"]]
        } else if (input$method == "shap"){
          ggplot(SHAP_ext, aes(x = reorder(lag, SHAP), y = SHAP)) +
            geom_bar(stat = "identity",
                     show.legend = FALSE,
                     fill = color,      # Background color
                     color = "white") + # Border color
            xlab("Lag") +
            ylab("SHAP") +  coord_flip()
          
        }
      })
    }
  )
}
