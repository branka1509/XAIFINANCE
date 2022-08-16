# plots for the loan_data
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/05_data_summary_and_visualization.Rmd

loanDataUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Data Exploration"))),
      fluidRow(
        column(12,
               box(
                 title = "Numeric Variables",
                 dataTableOutput(ns("num_table")),
                 width = 12
               ))
      ),
      fluidRow(
        column(12,
               box(
                 title = "Categorical Variables",
                 dataTableOutput(ns("cat_table")),
                 width = 12
               ))
      ),
      fluidRow(
        column(12,
               box(
                 title = "Variable Correlations",
                 plotOutput(ns("corr_plot"), height = "auto"),
                 width = 12
               )),
      ),
      fluidRow(
        column(12,
               box(
                 title = "Correlation Chart",
                 plotOutput(ns("corr_chart")),
                 width = 12
               ))
      ),
    )
  ) 
}

loanDataServer <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      # load the dataset
      df <- readRDS("data/clean/df_post_data_explor.rds")
      # dictionary <- read_excel("data/clean/LCDataDictionary.xlsx")
      loan_columns <- colnames(df[, !names(df) %in% c("loan_status")])
      # calculate correlations
      correlations <- df %>% 
        keep(is.numeric) %>%
        cor()
      numeric <- df %>% keep(is.numeric)
      p_value_mat <- ggcorrplot::cor_pmat(numeric)
      
      key_vars = df[, c("loan_amnt", "annual_inc", "dti", "fico_range_low", "revol_bal")]
      
      output$num_table <- renderDataTable({select_if(df,is.numeric) %>% sumtable(out = "return")})
      output$cat_table <- renderDataTable({select_if(df,is.factor) %>% sumtable(out = "return")})
      output$corr_plot <- renderPlot({ggcorrplot(correlations, type = "lower", p.mat = p_value_mat)}, height = 1000)
      output$corr_chart <- renderPlot({chart.Correlation(key_vars, histogram=TRUE, pch=19)})
    }
  )
}
