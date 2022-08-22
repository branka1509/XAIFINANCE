# plots for the loan_data
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/05_data_summary_and_visualization.Rmd

loanDataMultipleUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,
            h2("Multiple Data Exploration"))
      ),
      fluidRow(
        box(width=12,
            textOutput(ns("overview")))
      ),
      fluidRow(
        column(6,
               box(
                 uiOutput(ns("input_var_1")),
                 width = 12)),
       column(6,
              box(
                uiOutput(ns("input_var_2")),
                width = 12))
               
        ),
      fluidRow(
        column(6,
               box(width = 12, plotOutput(ns("plot_1")))),
        column(6,
               box(width = 12, plotOutput(ns("plot_2")))))
      ),
    fluidRow(
      column(12,
             box(width = 12, uiOutput(ns("dynamic_plot"))))
      ),
    )
}

loanDataMultipleServer <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      # load the dataset
      df <- readRDS("data/clean/df_post_data_explor.rds")
      loan_columns <- colnames(df[, !names(df) %in% c("loan_status")])
      
      # calculate correlations
      correlations <- df %>% 
        keep(is.numeric) %>%
        cor()
      numeric <- df %>% keep(is.numeric)
      numeric_var_1 <- reactive({is.numeric(df[,input$loan_var_1])})
      numeric_var_2 <- reactive({is.numeric(df[,input$loan_var_2])})
      var1 <- reactive({input$loan_var_1})
      var2 <- reactive({input$loan_var_2})
      p_value_mat <- ggcorrplot::cor_pmat(numeric)
      
      key_vars = df[, c("loan_amnt", "annual_inc", "dti", "fico_range_low", "revol_bal")]
      plotHeight <- reactive({
        if(numeric_var_1() == numeric_var_2()){
          return(360)
        } else {
          return(1080)
        }
      })
      # select the input variables
      output$input_var_1 <- renderUI({
        selectInput(
          ns("loan_var_1"), 
          label = "Select your primary variable",
          choices = append("loan_status",loan_columns)
        )
      })
      output$input_var_2 <- renderUI({
        selectInput(
          ns("loan_var_2"), 
          label = "Select your secondary variable",
          choices = loan_columns
        )
      })
      output$plot_1 <- renderPlot({
        # Input 1 numeric
        if(numeric_var_1() == FALSE){
          ggplot(df, aes_string(x = input$loan_var_1,
                         y = "(..count../sum(..count..)*100)")) +  # ..count.. -- is a special variable representing the frequency within each category
            geom_bar(fill = "lightblue") +
            labs(title = "Frequency of the selected variable",
                 x = colnames(df[var1()]),
                 y = "Frequency")
          # input 1 categorical
        } else {
          Desc(df[input$loan_var_1], plotit = TRUE)
        }
      })
      output$plot_2 <- renderPlot({
        # input 2 numeric
        if (numeric_var_2() == FALSE){
          ggplot(df, aes_string(x = input$loan_var_2,
                         y = "(..count../sum(..count..)*100)")) +  # ..count.. -- is a special variable representing the frequency within each category
            geom_bar(fill = "lightblue") +
            labs(title = "Frequency of the selected variable",
                 x = colnames(df[input$loan_var_2]),
                 y = "Frequency")
          # input 2 categorical
        } else {
          Desc(df[input$loan_var_2], plotit = TRUE)
        }
      })
      output$stacked_scatter <- renderPlot({
        if((numeric_var_1() == TRUE) && (numeric_var_2() == TRUE)){
          ggplot(data = df,
                 mapping = aes_string(x = input$loan_var_1, 
                               y = input$loan_var_2)) +
            geom_point(size = 2.5, 
                       shape = "diamond") +
            geom_smooth(method = "lm", se = F) +
            labs(title = "Relationship between variables",
                 x = colnames(df[input$loan_var_2]),
                 y = colnames(df[input$loan_var_1]))
        } else if((numeric_var_1() == FALSE) & (numeric_var_2() == FALSE)){
          # Stacked barplot 
          plotdata <- df %>%
            group_by(df[input$loan_var_2], df[input$loan_var_1])  %>%
            dplyr::summarize(n = n()) %>% 
            mutate(pct = n/sum(n),
                   lbl = scales::percent(pct))

          # !!! Here I cannot find away to use var1 and var2 in the plot!!! 
          ggplot(plotdata,                                                         
                 aes_string(x = input$loan_var_2,
                     y = "pct",                                                     
                     fill = input$loan_var_1)) + 
            geom_bar(stat = "identity",                                       
                     position = "fill") +
            geom_text(aes(label = lbl),
                      size = 3, 
                      position = position_stack(vjust = 0.5)) +
            scale_fill_brewer(palette = "Set2") +
            theme_minimal() + 
            labs (title = "Grouped barplot per loan status", x = colnames(df[input$loan_var_1]))
        } else {
          var1 <- input$loan_var_1
          var2 <- input$loan_var_2
          plotdata <- df %>%
            group_by_(input$loan_var_2) %>%
            summarise_(n = "n()",
                      mean = paste0("mean(",input$loan_var_1,")"))
          print(colnames(plotdata))
          grid.arrange(
          # mean plot
          ggplot(plotdata,
                 aes_string(x = paste0("factor(",input$loan_var_2,")"),
                     y = "mean")) +
            scale_y_continuous(breaks = seq(0, 8, 2)) + 
            geom_bar(stat = "identity", fill="steelblue"),
          
          ggplot(df, 
                 aes_string(x = input$loan_var_1, 
                     y = input$loan_var_2,
                     fill = input$loan_var_2)) +
            geom_density_ridges(alpha = 0.8),

          ggplot(df, 
                 aes_string(x = paste0("factor(",input$loan_var_2,")"),
                     y = input$loan_var_1,
                     color = input$loan_var_2)) +
            geom_boxplot(size = 1,                         # size of the box plot
                         outlier.shape = 1,                # shape of the outline
                         outlier.color = "black",          # color of the box plot
                         outlier.size  = 3),               # size of the outline
          ncol=1) # end grid arrange
        }
      })
      output$dynamic_plot <- renderUI({
        plotOutput(ns("stacked_scatter"), height = plotHeight())
      })
      output$overview <- renderText({
        "This tab allows you to explore bivariate graphs which in turn display the relationship between two variables. The type of graph will depend on the measurement level of the variables (whether the variables are categorical or quantitative)."
      })
    }
  )
}
