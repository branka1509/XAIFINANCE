# plots for the loan_data
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/05_data_summary_and_visualization.Rmd

loanExplainabilityUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Data Explainability"))),
      fluidRow(
        column(6,
               box(
                 uiOutput(ns("model")),
                 width = 12)
        ),
        column(6,
               box(
                 uiOutput(ns("score")),
                 width = 12)
        )
      ),
      fluidRow(
        box(width=12,h3("Global Explanations"))),
      fluidRow(
        column(6,
               box(width=12,
                   plotOutput(ns("feat_importance")))
        ),
        column(6,
               box(width=12,
                   textOutput(ns("feat_importance_int"))))
      ),
      fluidRow(
        column(6,
               box(width=12,
                   plotOutput(ns("shap")))
        ),
        column(6,
               box(width=12,
                   textOutput(ns("shap_int"))))
      ),
      fluidRow(
        column(6,
               box(width=12,
                   plotOutput(ns("pdp")))
        ),
        column(6,
               box(width=12,
                   textOutput(ns("pdp_desc"))))
      ),
      fluidRow(
        box(width=12,h3("Local Explanations"))),
      fluidRow(
        column(6,
               box(width=12,
                   uiOutput(ns("row")))
        ),
        column(6,
               box(width=12,
                   plotOutput(ns("row_shap"))))
      ),
      fluidRow(
        column(6,
               box(width=12,
                   plotOutput(ns("ice")))
        ),
        column(6,
               box(width=12,
                   dataTableOutput(ns("row_table"))))
      ),
    )
  )
}

loanExplainabilityServer <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      model <- reactive({
        get(paste0("exp_", input$model,"_",input$score))
      })
      exp_model <- reactive({
        get(paste0("rows_", input$model,"_",input$score))
      })
      selected_row <- reactive({as.numeric(input$row)})
      ns <- NS(id)
      output$model <- renderUI({
        selectInput(
          ns("model"), 
          label = "Select the model",
          choices = c(
            "All" = "best",
            "Stacked Ensemble" = "SE",
            "Gradient Boosting" = "GBM",
            "Random Forest" = "DRF",
            "Deep Learning" = "DL",
            "Generalized Linear Model" = "GLM",
            "XGBoost" = "XGB"),
          selected = "All",
        )
      })
      output$score <- renderUI({
        selectInput(
          ns("score"), 
          label = "Select your secondary variable",
          choices = c("Area under curve" = "auc",
                      "Log-Loss" = "logloss",
                      "Area under Curve Precision Recall" = "aucpr",
                      "Mean per Class Error" = "mean_per_class_error"),
          selected = "Area under curve"
        )
      })
      output$feat_importance <- renderPlot({
        model()[["varimp"]][["plots"]]
      })
      output$feat_importance_int <- renderPrint({
        print("Feature Importance Interpretation to be added")
      })
      output$feat_importance_desc <- renderPrint({
        model()[["varimp"]][["description"]][[1]][1]
      })
      output$shap <- renderPlot({
        model()[["shap_summary"]]$plots
      })
      output$shap_int <- renderPrint({
        print("Test for shap interpretation")
      })
      output$shap_desc <- renderPrint({
        print(model()[["shap_summary"]]$description[[1]][1])
      })
      output$pdp <- renderPlot({
        model()[["pdp"]][["plots"]][[1]]
      })
      output$pdp_desc <- renderPrint({
        print(model()[["pdp"]][["description"]][[1]][1])
      })
      output$row <- renderUI({
        selectInput(
          ns("row"),
          label = "Select row to analyze", choices = c(1:100)
        )
      })
      output$row_shap <- renderPlot({
        exp_model()[[selected_row()]][["shap_explain_row"]]
      })
      output$row_table <- renderDataTable({
        datatable(as.data.frame(exp_model()[[selected_row()]][["shap_explain_row"]][["plots"]][[1]][["data"]][1]))
      })
      output$ice <- renderPlot({
        exp_model()[[selected_row()]][["ice"]]
      })
    }
  )
}
