# loan models example code
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/06_training_models_and_explainability.Rmd
loanModelsUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width = 12,
            h2("Models"))
      ),
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
        column(12,
               box(collapsible = TRUE,
                   title = "Model Summary",
                   verbatimTextOutput(ns("model_summary")),
                   width = 12)
        ),
      ),
      fluidRow(
        column(6,
               box(collapsible = TRUE,
                   title = "Performance",
                   verbatimTextOutput(ns("model_performance")),
                   width = 12)
        ),
        column(6,
               box(
                 title = "Performance Table Interpretation",
                 textOutput(ns("model_performance_int")),
                 width = 12)
        )
      ),
      fluidRow(
        column(6,
               box(collapsible = TRUE,
                   title = "Confusion Matrix",
                   verbatimTextOutput(ns("model_conf_matrix")),
                   width = 12)
        ),
        column(6,
               box(
                 title = "Confusion Matrix Interpretation",
                 textOutput(ns("model_conf_matrix_int")),
                 width = 12)
        )
      ),
      fluidRow(
        column(6,
               box(collapsible = TRUE,
                 title = "Receiver Operating Characteristic",
                 plotOutput(ns("perf_roc")),
                 width = 12)
        ),
        column(6,
               box(
                 title = "Receiver Operating Characteristic Interpretation",
                 textOutput(ns("perf_roc_int")),
                 width = 12)
        )
        ),
      fluidRow(
        column(6,
               box(collapsible = TRUE,
                   title = "Precision Recall",
                   plotOutput(ns("perf_pr")),
                   width = 12)
        ),
        column(6,
               box(
                 title = "Precision Recall Interpretation",
                 textOutput(ns("perf_pr_int")),
                 width = 12)
        )
      ),
      fluidRow(
        column(6,
               box(
                 title = "Confusion Matrix",
                 tableOutput(ns("conf_matrix")),
                 width = 12)
        ),
        column(6,
               box(
                 title = "Summary",
                 tableOutput(ns("threshold")),
                 width = 12)
        )
      )
    )
  )
}

loanModelsServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      # load the dataset
      df <- readRDS("data/clean/df_post_data_explor.rds")
      
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
      output$perf_roc <- renderPlot({
        plot(get(paste0("perf_", input$model,"_",input$score)), type = "roc")
      })
      output$perf_roc_int <- renderPrint({
        cat("In order to interpret the ROC curve, we assume a baseline model which is a random classifier – expected to give points lying along the diagonal (FPR = TPR). The closer the curve comes to the 45-degree diagonal of the ROC space, the less accurate the test. In this context, we observe that the model performance better than the baseline. ")
      })
      output$perf_pr <- renderPlot({
        plot(get(paste0("perf_", input$model,"_",input$score)), type = "pr")
      })
      output$perf_pr_int <- renderPrint({
        cat("Precision-Recall is a useful measure of success of prediction when the classes are very imbalanced. In information retrieval, precision is a measure of result relevancy, while recall is a measure of how many truly relevant results are returned. Put differently, the precision-recall curve shows the trade-off between precision and recall for different threshold. A high area under the curve represents both high recall and high precision, where high precision relates to a low false positive rate, and high recall relates to a low false negative rate. High scores for both show that the classifier is returning accurate results (high precision), as well as returning a majority of all positive results (high recall).")
      })
      output$conf_matrix <- renderTable({
        print(get(paste0("conf_matrix_", input$model,"_",input$score)))
      })
      output$model_conf_matrix <- renderPrint({
        get(paste0(input$model,"_",input$score))@model[["training_metrics"]]@metrics[["cm"]][["table"]]
        
      })
      output$model_conf_matrix_int <- renderPrint({
        model <- get(paste0(input$model,"_",input$score))
        cat("In this context, the confusion matrix indicates that out of ", model@model[["training_metrics"]]@metrics[["cm"]][["table"]][3,1] + model@model[["training_metrics"]]@metrics[["cm"]][["table"]][3,2] ,"predictions made the model has correctly labeled the loan contract in ", model@model[["training_metrics"]]@metrics[["cm"]][["table"]][1,1] + model@model[["training_metrics"]]@metrics[["cm"]][["table"]][2,2], 
            "cases. Furthermore, the model makes", model@model[["training_metrics"]]@metrics[["cm"]][["table"]][2,1], "false positive (eg. the model ranks a good loan as bad performing) predictions and ", model@model[["training_metrics"]]@metrics[["cm"]][["table"]][1,2] ,"false negative (eg. the model ranks a bad performing loan as good) predictions. The threshold used in 
    this case is the F1-optimal threshold which is equal to", model@model[["cross_validation_metrics"]]@metrics[["max_criteria_and_metric_scores"]][1,2])
        
      })
      # output$model_summary <- renderPrint({
      #   print(summary(paste0(input$model,"_",input$score)))
      # })
      output$threshold <- renderTable({
        print(get(paste0("threshold_", input$model,"_",input$score)))
      })
      output$model_summary <- renderPrint({
        get(paste0(input$model,"_",input$score))@model[["model_summary"]]
      })
      output$model_performance <- renderText({
        paste0("AUC: ", best_auc@model[["cross_validation_metrics"]]@metrics[["pr_auc"]], "\n",
            "AUCPR: ", get(paste0(input$model,"_",input$score))@model[["cross_validation_metrics"]]@metrics[["pr_auc"]], "\n",
            "LogLoss: ",get(paste0(input$model,"_",input$score))@model[["cross_validation_metrics"]]@metrics[["logloss"]], "\n",
            "Mean error per class: ", get(paste0(input$model,"_",input$score))@model[["cross_validation_metrics"]]@metrics[["mean_per_class_error"]], "\n",
            "Gini: ", get(paste0(input$model,"_",input$score))@model[["cross_validation_metrics"]]@metrics[["Gini"]])
      })
      output$model_performance_int <- renderText({
        paste0("Looking at the AUC performance measures the model reports at AUC of ", get(paste0(input$model,"_",input$score))@model[["cross_validation_metrics"]]@metrics[["AUC"]],
            " which according to general rule of thumb, falls with the acceptable range of accuracy (AUC = 0 indicates a perfectly inaccurate test; AUC = 0.5 indicates no discrimination; AUC = 0.7-0.8 indicates an acceptable level of accuracy; AUC = 0.8-0.9 is considered excellent, and AUC > 0.9 is considered outstanding. The AUC value in this context is obtained through a 5-fold cross-validation on the training data and computed through combined holdout predictions. \n
The AUC measure is classification-threshold-invariant, meaning it reflects the predictiveutility of the model regardless of the classification threshold chosen. This property may limit it usefulness  in certain cases in which there is a wide disparity between the cost of false negative (eg. the model ranks a bad performing loan as good) vs false positive (eg. the model ranks a good loan as bad performing). This is specifically relevant in a loan performance use case as one likely wants to prioritize minimizing the false negatives (i.e. the actual loss incurred) even if this results in an increase in the false positives. \n
Precision-Recall is a useful measure of success of prediction when the classes are very imbalanced. In information retrieval, precision is a measure of result relevancy, while recall is a measure of how many truly relevant results are returned. Put differently, the precision-recall curve shows the trade-off between precision and recall for different threshold. A high area under the curve represents both high recall and high precision, where high precision relates to a low false positive rate, and high recall relates to a low false negative rate. High scores for both show that the classifier is returning accurate results (high precision), as well as returning a majority of all positive results (high recall). \n
The area under the precision-recall curve is ", get(paste0(input$model,"_",input$score))@model[["cross_validation_metrics"]]@metrics[["pr_auc"]], " which is better that a random classifier but could be improved. What we can interpret from the plot is that the model has high precision (>.7) only for low levels of recall and vice versa. \n
The log-loss indicators measure how close a predicted probability is to the corresponding true class. The larger this difference, the higher the log-loss value. The log-loss value for the model is ", best_auc@model[["cross_validation_metrics"]]@metrics[["logloss"]])

      })
    }
  )
}