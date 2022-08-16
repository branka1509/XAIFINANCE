# plots for the loan_data
# https://github.zhaw.ch/IWA/xai/blob/master/2022_repo/Loan%20Performance%20Use%20Case/05_data_summary_and_visualization.Rmd

tsClassicUi <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = id,
    fluidPage(
      fluidRow(
        box(width=12,h2("Time Series Classic XAI"))),
      fluidRow(
        column(width = 6,
               box(width = 12,
                   plotOutput(ns("varimp")))
        ),
        column(6,
               box(width = 12,
                   plotOutput(ns("pdp"))))
      ),
      fluidRow(
        column(width = 6,
               box(width = 12,
                   plotOutput(ns("ice")))
        ),
        column(6,
               box(width = 12, 
                   plotOutput(ns("exp_shap"))))
      ),
      fluidRow(
        column(12,
               box(width = 12,
                   textOutput(ns("text"))))
      ),
    )
  )
}

tsClassicServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      x = na.omit(diff(log(dat$Bid)))
      data_mat = cbind(x,lag.xts(x),lag.xts(x,k=2),lag.xts(x,k=3),lag.xts(x,k=4),lag.xts(x,k=5),lag.xts(x,k=6))

      data_mat = na.exclude(data_mat)

      # Train and test separator 
      in_out_sample_separator="2015-06-01"
      
      # Scaling data for the neural net
      maxs = apply(data_mat, 2, max)
      mins = apply(data_mat, 2, min)
      
      # Transform data into [0,1] and check whether transformation worked 
      scaled = scale(data_mat, center = mins, scale = maxs - mins)
      apply(scaled,2,min)
      apply(scaled,2,max)
      
      
      # Create a test and train set
      train_set = scaled[paste("/",in_out_sample_separator,sep=""),]
      test_set = scaled[paste(in_out_sample_separator,"/",sep=""),]
      
      # Set objects as data frames
      train_set = as.data.frame(train_set)
      test_set = as.data.frame(test_set)
      
      # Rename colnames of train and test set
      colnames(train_set) = paste("lag",0:(ncol(train_set)-1),sep="")
      colnames(test_set) = paste("lag",0:(ncol(train_set)-1),sep="")

      
      # Create h2o environments for the train and test set
      test = as.h2o(test_set)
      train = as.h2o(train_set)

      dl = h2o.deeplearning(x = 2:7,
                            y = "lag0",
                            hidden = c(3,2),
                            epochs = 1000,
                            seed = 1,
                            training_frame = train)

      # Eval performance
      perf = h2o.performance(dl)

      # Generate predictions on a test set (if necessary):
      pred = h2o.predict(dl, newdata = test)
      
      eval = as.data.frame(pred)
      eval$real = test_set$lag0
      
      
      # Explain the model using classic XAI
      # Explain a model
      classic = h2o.explain(dl, test)

      # Train a nn model using the neuralnet function 
      train_set=as.matrix(train_set)
      test_set=as.matrix(test_set)
      colnames(train_set)=paste("lag",0:(ncol(train_set)-1),sep="")
      n = colnames(train_set)
      
      
      # Model: target is current bitcoin, all other variables are explanatory
      f = as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))
      
      # Set/fix the random seed and train the neural net 
      set.seed(1)
      nn = neuralnet(f,data=train_set,hidden=c(3,2),linear.output=F)
      
      # Obtain local-SHAP
      exp_nn = explain(nn, data = train_set)
      new = as.data.frame(test[1,])
      exp_nn_shap = shap(exp_nn, new_observation = new)
      
      output$varimp <- renderPlot({
        classic[["varimp"]]
      })
      output$pdp <- renderPlot({
        classic[["pdp"]]
      })
      output$ice <- renderPlot({
        classic[["ice"]]
      })
      output$exp_shap <- renderPlot({
        plot(exp_nn_shap)
      })
      output$text <- renderDataTable({
        cat("Problem: Perturbation-based methods are fully dependent on the ability to perturb samples in a meaningful way!")
      })
    }
  )
}
