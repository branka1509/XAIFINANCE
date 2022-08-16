# plots for the loan_data
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/05_data_summary_and_visualization.Rmd

loanStabilityUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Loan Stability"))
      ),
      fluidRow(
        column(6,
               box(
                 uiOutput(ns("feature")),
                 width = 12)),
        column(6,
               box(
                 uiOutput(ns("manipulation")),
                 width = 12))
        
      ),
      fluidRow(
        column(6,
               box(width = 12, dataTableOutput(ns("table")))
        ),
        column(6,
               box(
                 textOutput(ns("interp")),
                 width = 12)
        ),
      ),
    ),
  )
}

loanStabilityServer <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      stability <- reactive({
        feat <- as.numeric(input$feature)
        get(input$manipulation)[[feat]][[1]]})
      # select the input variables
      output$feature <- renderUI({
        selectInput(
          ns("feature"), 
          label = "Select your feature",
          choices = c(
            "loan_amnt" = "1",
            "annual_inc" = "2",
            "dti" = "3",
            "fico_range_low" = "4",
            "num_tl_op_past_12m" = "5",
            "acc_open_past_24mths" = "6"
          )
        )
      })
      output$manipulation <- renderUI({
        selectInput(
          ns("manipulation"), 
          label = "Select your manipulation",
          choices = c("Sensitivity 1" = "sensitivities_1",
                      "Sensitivity 2" = "sensitivities_2",
                      "Sensitivity 3" = "sensitivities_3",
                      "Sensitivity 4" = "sensitivities_4",
                      "Sensitivity 5" = "sensitivities_5",
                      "Sensitivity 6" = "sensitivities_6",
                      "Sensitivity 7" = "sensitivities_7",
                      "Sensitivity 8" = "sensitivities_8",
                      "Sensitivity 9" = "sensitivities_9",
                      "Sensitivity 10" = "sensitivities_10")
        )
      })
      output$table <- renderDataTable({
        DT::datatable(stability())
      })
      output$interp <- renderPrint({
        cat("In this context, the relative change of the PD with respect to the change in the variable is ",
            stability()[3]/stability()[4], ". Put differently, for a change in the variable of", stability()[4], 
            " the estimated PD changed by: ", stability()[3], 
            ". Moreover, the same change in the variable, resulted in a change of the overall predictive utility of the model by ", 
            stability()[1]/stability()[4], ". The slight change in the input feature further resulted in ", stability()[7], 
            " loan contracts being reclassifed. Finally, we fit a linear model to estimate the impact of the change of the variable to the change in the prediction. The results indicate a coefficient of ", 
            stability()[9], "and a p-value of ", stability()[10])
      })
    }
  )
}
