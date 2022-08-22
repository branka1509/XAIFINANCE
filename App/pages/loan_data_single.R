# plots for the loan_data
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/05_data_summary_and_visualization.Rmd

loanDataSingleUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Single Data Exploration"))
      ),
      fluidRow(
        box(width=12,textOutput(ns("overview")))
      ),
      fluidRow(
        column(6,
               box(
                 title = "Variable",
                 uiOutput(ns("input_var")),
                 width = 12)
        ),
        column(6,
               box(width = 12, title = "Description", textOutput(ns("description")))
        )
      ),
        
      fluidRow(
        column(6,
               box(
                 dataTableOutput(ns("var_descr")),
                 width = 12)
        ),
        column(6,
               box(width = 12, 
                   plotOutput(ns("var_plot")))
        )
      ),
      fluidRow(
        column(6,
               box(width = 12, plotOutput(ns("box_plot")))
        ),
        column(6,
               box(
                 plotOutput(ns("dens_plot")),
                 width = 12)
        ),
      ),
      fluidRow(
        column(6, align="center",
               box(
                 uiOutput(ns("sum_fit")),
                 width = 12)
        ),
        column(6, align="center",
               box(
                 uiOutput(ns("det_fit")),
                 width = 12)
        )
      ),
    )
  ) 
}

loanDataSingleServer <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      # check if variable is numeric
      numeric_var <- reactive({is.numeric(df[,input$loan_var])})
      # load the dataset
      df <- readRDS("data/clean/df_post_data_explor.rds")
      dictionary <- read_excel("data/clean/LCDataDictionary.xlsx")
      # df <- df %>% 
      #   mutate(loan_status = recode(loan_status, 
      #                               "0" = 0, 
      #                               "1" = 1))
      df$loan_status = as.factor(df$loan_status)
      
      plot_var <- reactive({df %>%
        count(df[input$loan_var])})
      
      plotdata <- reactive({df %>%
        group_by(df[input$loan_var], loan_status)  %>%
        dplyr::summarize(n = n()) %>% 
        mutate(pct = n/sum(n),
               lbl = scales::percent(pct))})
      
      loan_columns <- colnames(df)
      
      fit <- reactive({
        form <- sprintf("%s~%s","loan_status",paste0(input$loan_var,collapse="+"))
        glm(form, data = df, family = "binomial")})
      
      # select the input variables
      output$input_var <- renderUI({
        selectInput(
          ns("loan_var"), 
          label = "Select your primary variable",
          choices = loan_columns
        )
      })
      # description for numeric and categorical
      output$description <- renderText({dictionary$Description[dictionary$LoanStatNew == input$loan_var]})

      output$var_descr <- renderDataTable({
        if (numeric_var()){
          # numeric 
          # describe(df[input$loan_var])
          summary_df <- as.data.frame(summary(df[input$loan_var]))
          summary_df$Var1 <- NULL
          summary_df$Var2 <- NULL
          DT::datatable(summary_df)
        } else {
          # categorical
          DT::datatable(as.data.frame(table(df[input$loan_var])))}
        })
      
      output$var_plot <- renderPlot({
        if (numeric_var()){
          # numeric
          Desc(df[input$loan_var], plotit = TRUE)}
        else {
          # categorical
          # piechart
          ggplot(plot_var(), 
                 aes(x = "", 
                     y = n, 
                     fill = plot_var()[,input$loan_var],
                     label = plot_var()[,input$loan_var])) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = n ),  
                      position = position_stack(vjust = 0.5)) +
            theme_classic() +
            theme(plot.title = element_text(hjust=0.5),
                  axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank()) +
            labs(fill = "Class",
                 x = NULL,
                 y = NULL,
                 title = "Pie chart of the selected variable") + 
            coord_polar("y")
        }
      })
      output$box_plot <- renderPlot({
        if (numeric_var()){
          ggplot(df, aes(x = loan_status, 
                         y = df[,input$loan_var],
                         fill = loan_status)) +
            geom_boxplot(notch = TRUE,
                         alpha = .5) +
            labs(title = "Boxplots of the selected variable per the loan status", 
                 x = "Loan status", y=colnames(df[input$loan_var]))}
        else {
          # treemap
          ggplot(plot_var(),
                 aes(fill = plot_var()[,input$loan_var], 
                     area = n,
                     label = plot_var()[,input$loan_var])) +
            geom_treemap() +
            geom_treemap_text(colour = "white",
                              place = "centre")
          }})
      
      output$dens_plot <- renderPlot({
        if (numeric_var()){
          ggplot(df, 
                 aes(x = df[,input$loan_var], 
                     y = loan_status,
                     fill = loan_status)) +
            geom_density_ridges(alpha = 0.7) + 
            theme_ridges() +
            theme(legend.position = "none") + 
            labs(title = "Density plots of the selected variable per the loan status", y = "Loan status", x=colnames(df[input$loan_var]))} 
        else {
          ggplot(plotdata(),                                                         
                 aes(x = input$loan_var,
                     y = pct,                                                     
                     fill = loan_status)) + 
            geom_bar(stat = "identity",                                       
                     position = "fill") +
            geom_text(aes(label = lbl),                                                                                       
                      size = 3, 
                      position = position_stack(vjust = 0.5)) +
            scale_fill_brewer(palette = "Set2") +
            theme_minimal() + 
            labs (title = "Grouped barplot per loan status", x = colnames(df[input$loan_var]))
          }})

      
      output$sum_fit <- renderUI({HTML(tab_model(fit())$knitr)})
      
      output$det_fit <- renderUI({reg <- fit() %>% export_summs(number_format = "%.2f") 
                                  HTML(huxtable::to_html(reg))})
      output$overview <- renderText("This tab allows you to explore a single feature included in the dataset. Specifically, you are able to explore univariate graphs which in turn plot the distribution of data from a single variable. The variable can be categorical (e.g., loan purpose, loan status) or quantitative (e.g., annual income, loan amount). Furthermore, the tab allows you to print basic statistics for the selected variable and observe the dependence of the selected feature with the key variable of interest (the status of the loan).")
      }
      )
}
