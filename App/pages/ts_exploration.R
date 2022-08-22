# plots for the loan_data
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/05_data_summary_and_visualization.Rmd

tsExplorationUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Time Series Data Explainability"))),
      fluidRow(
        box(width=12,textOutput(ns("overview")))),
      fluidRow(
        box(width=12,h3("Original data"))),
      fluidRow(
        column(width = 6,
          box(width = 12,
              dataTableOutput(ns("head")))
        ),
        column(6,
               box(width = 12,
                    dataTableOutput(ns("tail"))))
      ),
      fluidRow(
        column(width = 6,
               box(width = 12,
                   plotOutput(ns("ts_plot")))
        ),
        column(6,
               box(width = 12, 
                   textOutput(ns("ts_desc"))))
      ),
      fluidRow(
        box(width=12,h3("Transformed features"))),
      fluidRow(
        column(width = 6,
               box(width = 12,
                   dataTableOutput(ns("head_mat")))
        ),
        column(6,
               box(width = 12,
                   dataTableOutput(ns("tail_mat"))))
      ),
    )
  )
}

tsExplorationServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      x <- na.omit(diff(log(dat$Bid)))
      data_mat <- x
      data_mat <- cbind(x,lag.xts(x),lag.xts(x,k=2),lag.xts(x,k=3),lag.xts(x,k=4),lag.xts(x,k=5),lag.xts(x,k=6))
      data_mat <- na.exclude(data_mat)
      
      output$head <- renderDataTable({
        datatable(as.data.frame(head(dat)))
      })
      output$tail <- renderDataTable({
        datatable(as.data.frame(tail(dat)))
      })
      output$ts_plot <- renderPlot({
        p1 <- ggplot(dat, aes(x = Index, y = Bid)) + geom_line() + ggtitle("Prices")
        p2 <- ggplot(log(dat$Bid), aes(x = Index, y = Bid)) + geom_line() + ggtitle("Log Prices")
        p3 <- ggplot(diff(log(dat$Bid)), aes(x = Index, y = Bid)) + geom_line() + ggtitle("Log Returns")
        p4 <- ggplot(log(dat$Volume), aes(x = Index, y = Volume)) + geom_line() + ggtitle("Log Volume")
        grid.arrange(p1,p2,p3,p4, nrow = 2)
      })
      output$ts_desc <- renderPrint({
        cat("The price series (top panels) are non-stationary and the log-returns (bottom panels) show 
        evidence of vola-clustering or conditional heteroscedasticity. Furthermore, the BTC is subject 
        to more frequent extreme events and more pronounced up- and down-turns")
      })
      output$head_mat <- renderDataTable({
        as.data.frame(head(data_mat)) %>%
          datatable() %>%
          formatRound(columns=colnames(data_mat), digits=4)
      })
      output$tail_mat <- renderDataTable({
        as.data.frame(tail(data_mat)) %>%
          datatable() %>%
          formatRound(columns=colnames(data_mat), digits=4)
      })
      output$overview <- renderText("The data used in this use case contains daily Bitcoin returns covering the period15-04-2014 to 30-06-2021. This tab allows you to explore the original data as well as visualize the time series of the prices, the log prices, the log returns, and the log volume.")
    }
  )
}
