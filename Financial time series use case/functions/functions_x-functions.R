

compute_trade_perf_LPD_Hessian_func<-function(x,y, updated_params, layer_size,linear_output,atan_not_sigmoid)
{

# Net output
  fwd <- forwardPropagation(x, updated_params, layer_size,linear_output,atan_not_sigmoid)
  output<-as.double(fwd$A_list[[length(fwd$A_list)]])
# Trading returns: sign rule: output is already lagged by one day (i.e. not additional lagging required here)
  perf_sign<-as.double(y)*sign(output)
# Trading returns: proportionality rule
  perf_prop<-as.double(y)*output/mean(output)
# LPD
  LPD_obj<-LPD(fwd, updated_params, list_layer_size,linear_output,atan_not_sigmoid)
# Compute intercept
  intercept<-as.double(fwd$A_list[[length(fwd$A_list)]])-apply(LPD_obj$LPD_t*x,1,sum)
  LPD_with_intercept<-cbind(intercept,LPD_obj$LPD_t)
# Hessian
  dA_list<-LPD_obj$dA_list
  LPD_forward_obj<-LPD_forward(fwd, updated_params, list_layer_size,linear_output,atan_not_sigmoid)
  dA_list_forward<-LPD_forward_obj$dA_list_forward
# Hessian_diag computes the whole diagonal of the Hessian matrix
  Hess_obj<-Hessian_diag(dA_list,fwd, updated_params, list_layer_size,linear_output,atan_not_sigmoid,dA_list_forward)
  QPD<-Hess_obj$Hessian_t

  return(list(output=output,perf_sign=perf_sign,perf_prop=perf_prop,LPD_with_intercept=LPD_with_intercept,QPD=QPD))

}


# Determine trading signals based on rolling quantiles
#   No cheating: real-time
#   xyz<-std_mean
# quantile_select<-0.5
weight_trade_func<-function(xyz,quantile_select,length_roll_quantile)
{
  # Old code based on median: newer code below is more general since it relies on quantiles
  if (F)
  {
    roll_median<-xyz
    # First possibility: from 1 to t i.e. 1:i for i=1,2,...,T
    for (i in 1:length(xyz))
    {
      roll_median[i]<-as.double(median(xyz[1:i]))
    }
    # Second possibility: rolling window of length length_roll_median
    roll_median<-rollmedian(xyz,k=length_roll_median)
  }

  # Third possibility: more general i.e. arbitrary quantile
  roll_quant_up<-apply.rolling(xyz,width=length_roll_quantile,FUN="quantile",p=quantile_select)
  roll_quant_low<-apply.rolling(xyz,width=length_roll_quantile,FUN="quantile",p=1-quantile_select)
  if (F)
  {
    # This is a check: last element of tail should match median since p=0.5
    tail(apply.rolling(xyz,width=151,FUN="quantile",p=0.5))
    median(xyz[(length(xyz)-151+1):length(xyz)])
  }
# Weight is one if xyz smaller than LAGGED quantile
# This might be preferable to UN-lagged rule below because a current negative outlier will pull-down the quantile, too, which is bad
# Note that since xyz[i] is not in lagged roll_quant[i] : therefore even if p=1 it could happen that weight_trade[i] is F
  weight_trade_up<-(xyz<=lag(roll_quant_up))
  weight_trade_low<-(xyz>=lag(roll_quant_low))
  weight_trade_two_side<-(xyz<=lag(roll_quant_up))&(xyz>=lag(roll_quant_low))
  if (F)
  {
# Weight is one if xyz smaller than UN-lagged quantile
    weight_trade_up<-(xyz<=(roll_quant_up))
    weight_trade_low<-(xyz>=(roll_quant_low))
    weight_trade_two_side<-(xyz<=(roll_quant_up))&(xyz>=(roll_quant_low))
  }
  return(list(weight_trade_up=weight_trade_up,weight_trade_low=weight_trade_low,weight_trade_two_side=weight_trade_two_side))

}




load_SP_func<-function(in_out_sample_separator,use_scaled_data)
{
  load(file = paste(getwd(),"/Data/SP.Rdata",sep=""))    #YM,URO,SF,NQ,ES,DX,CD,AD  is.xts(tsData)
  sp_data<-mydata
  lag_explanatory<-1
  if (F)
  {
    colo<-rainbow(ncol(sp_data))#c("black","red","blue","green","yellow")
    plot(scale(sp_data),col=colo)
    for (i in 1:ncol(sp_data))
      mtext(colnames(sp_data)[i],line=-10-i,col=colo[i])
  }
  sp_data<-xts(sp_data, as.POSIXct(index(sp_data), format="%Y%m%d %H:%M"))

  #------------------------------
  # Specify Data matrix
  exp_select<-c("SP monthly returns","Unrate","Payroll","Indpro","PMI")
  exp_select<-c("SP monthly returns","Unrate","Payroll","Indpro")

  data_mat<-na.exclude(cbind(sp_data[,"SP monthly returns"],lag(sp_data[,exp_select],k=lag_explanatory)))

  #--------------------------------------------------------------------

  target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
  tail(target_in)
  explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
  tail(explanatory_in)

  lm_obj<-lm(target_in~explanatory_in)

  summary(lm_obj)

  train<-cbind(target_in,explanatory_in)
  #-------------------------------------------

  #-------------------------------------------------------------------------------
  # Neural net fitting

  # Scaling data for the NN
  maxs <- apply(data_mat, 2, max)
  mins <- apply(data_mat, 2, min)
  # Transform data into [0,1]
  scaled <- scale(data_mat, center = mins, scale = maxs - mins)

  apply(scaled,2,min)
  apply(scaled,2,max)
  #-----------------
  # Train-test split
  # Train-test split
  train_set_xts <- scaled[paste("/",in_out_sample_separator,sep=""),]
  test_set_xts <- scaled[paste(in_out_sample_separator,"/",sep=""),]
  index(train_set_xts)

  train_set<-as.matrix(train_set_xts)
  test_set<-as.matrix(test_set_xts)
  #------------------------------------
  # Scaled data leads to stronger dynamics in the LPD: with single 100-layer or 3,2 layer

  if (use_scaled_data)
  {
    y_train<-as.matrix(train_set[,1],ncol=1)
    x_train<-train_set[,2:ncol(train_set)]
    y_test<-as.matrix(test_set[,1],ncol=1)
    x_test<-test_set[,2:ncol(test_set)]
  } else
  {
    y_train<-target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
    x_train<-explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
    y_test<-target_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),1]
    x_test<-explanatory_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),2:ncol(data_mat)]
  }
  head(x_train)
  tail(x_train)
  head(y_train)
  tail(y_train)
  head(x_test)
  tail(x_test)
  head(y_test)
  tail(y_test)
  return(list(x_train=x_train,x_test=x_test,y_train=y_train,y_test=y_test,train_set=test_set,train_set=test_set))
}



load_BTC_func<-function(in_out_sample_separator,use_scaled_data,load_data)
{
  # Load Bitcoin data
  path.dat<-paste(getwd(),"/Data/",sep="")
  #path.dat<-paste(path.main,"/Exercises/Erste Woche/Data/",sep="")


  if (load_data)
  {
    Quandl.api_key("yUtHvNxMBzGmnxKbQ79z")

    dat=Quandl(code = "BITSTAMP/USD",type="xts")
    summary(dat)
    tail(dat$Bid)
    print(path.dat)
    #save(dat,file=paste(path.dat,"bitcoin.Rdata",sep=""))
  } else
  {
    load(paste(path.dat,"/bitcoin.Rdata",sep=""))
  }
  #-------------------
  head(dat)
  tail(dat)
  #-------------------
  # Plot  data


  # plot last, bid and ask in single figure names(dat)
  par(mfrow=c(2,2))
  plot(dat$Bid,col=1,main="Prices")
  plot(log(dat$Bid),col=1,on=1,main="Log-prices")  #tail(dat$Bid)
  plot(diff(log(dat$Bid)),col=1,on=1,main="Log-returns")
  plot(log(dat$Volume),col=1,on=1,main="Log-volumes")


  #-----------------------------
  # Evidence: vola-clustering

  ########################################################################################-----------------------------
  # Fit a GARCH to log-returns

  x_fit<-as.ts(na.omit(diff(log(dat$Bid))))
  # GARCH(1,1)
  y.garch_11<-garchFit(~garch(1,1),data=x_fit,delta=2,include.delta=F,include.mean=F,trace=F)

  #  ts.plot(x_fit)
  #  lines(y.garch_11@sigma.t,col="red")
  #-----------------
  standard_residuals<-y.garch_11@residuals/y.garch_11@sigma.t
  #  ts.plot(standard_residuals)
  #-----------------
  #  par(mfrow=c(2,1))
  #  acf(x_fit,main="Acf log-returns",ylim=c(0,0.1))
  #  acf(standard_residuals,main="Acf standardized residuals GARCH(1,1)",ylim=c(0,0.1))

  ####################################################################
  # Classic linear regression: applied to Bid-prices
  # Specify target and explanatory data: we use first six lags based on above data analysis
  x<-ret<-na.omit(diff(log(dat$Bid)))
  x_level<-log(dat$Bid)
  #------------------
  data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6))
  # Check length of time series before na.exclude
  dim(data_mat)
  data_mat<-na.exclude(data_mat)
  # Check length of time series after removal of NAs
  dim(data_mat)
  head(data_mat)
  tail(data_mat)

  #--------------------------------------------------------------------
  # Specify in- and out-of-sample episodes

  target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
  tail(target_in)
  explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
  tail(explanatory_in)

  lm_obj<-lm(target_in~explanatory_in)

  summary(lm_obj)

  train<-cbind(target_in,explanatory_in)
  #-------------------------------------------

  #-------------------------------------------------------------------------------
  # Neural net fitting

  # Scaling data for the NN
  maxs <- apply(data_mat, 2, max)
  mins <- apply(data_mat, 2, min)
  # Transform data into [0,1]
  scaled <- scale(data_mat, center = mins, scale = maxs - mins)

  apply(scaled,2,min)
  apply(scaled,2,max)
  #-----------------
  # Train-test split
  # Train-test split
  train_set_xts <- scaled[paste("/",in_out_sample_separator,sep=""),]
  test_set_xts <- scaled[paste(in_out_sample_separator,"/",sep=""),]
  index(train_set_xts)

  train_set<-as.matrix(train_set_xts)
  test_set<-as.matrix(test_set_xts)

  if (use_scaled_data)
  {
    y_train<-target_in<-as.matrix(train_set[,1],ncol=1)
    x_train<-explanatory_in<-train_set[,2:ncol(train_set)]
    y_test<-target_out<-as.matrix(test_set[,1],ncol=1)
    x_test<-explanatory_out<-test_set[,2:ncol(test_set)]
  } else
  {
    y_train<-target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
    x_train<-explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
    y_test<-target_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),1]
    x_test<-explanatory_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),2:ncol(data_mat)]
  }

  return(list(x_train=x_train,x_test=x_test,y_train=y_train,y_test=y_test,train_set=train_set,test_set=test_set,dat=dat,target_in=target_in,explanatory_in=explanatory_in,target_out=target_out,explanatory_out=explanatory_out,data_mat=data_mat))

}







load_spdaily_func<-function(in_out_sample_separator,use_scaled_data,load_data,start_date,diff_data)
{
  # Load Bitcoin data
  path.dat<-paste(getwd(),"/Data/",sep="")
  #path.dat<-paste(path.main,"/Exercises/Erste Woche/Data/",sep="")


  if (load_data)
  {



    sp500 <- new.env()
    today<-Sys.Date()
    getSymbols("^GSPC", env = sp500, src = "yahoo",from = as.Date("1990-01-04"), to = today)
    GSPC<-sp500$GSPC
    head(GSPC)
    tail(GSPC)
    class(GSPC)


    getSymbols("^VIX", env = sp500, src = "yahoo",from = as.Date("1990-01-04"), to = today)

    VIX<-sp500$VIX
    head(VIX)
    tail(VIX)



    ###################################################
    ### code chunk number 2: slides_trading_indicators_univariate.Rnw:98-110
    ###################################################
    # Line Chart
    if (F)
    {
      chartSeries(log(GSPC),type="line",theme=chartTheme("white"))

      # Bar Chart
      chartSeries(AAPL, type="bar",subset="2007-05::2007-06",
                  theme=chartTheme("white"))

      # Candle Stick Chart
      chartSeries(AAPL,type="candlesticks",subset="2007-05",
                  up.col = "white",down.col = "black",theme=chartTheme("white"))
    }

    x<-log_sp500<-log(GSPC$GSPC.Close)
#    x<-na.exclude(diff(log_sp500))
    plot(x)
    save(x,file=paste(path.dat,"sp_daily.Rdata",sep=""))
  } else
  {
    load(paste(path.dat,"/sp_daily.Rdata",sep=""))
  }


  #-------------------
  head(x)
  tail(x)
  if (diff_data)
    x<-na.exclude(diff(x))
  tail(x)
  #-------------------
  # Plot  data


  #------------------
  data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5))
  # Check length of time series before na.exclude
  dim(data_mat)
  data_mat<-na.exclude(data_mat)
  # Check length of time series after removal of NAs
  dim(data_mat)
  head(data_mat)
  tail(data_mat)
  data_mat<-data_mat[paste(start_date,"/",sep="")]
  #--------------------------------------------------------------------
  # Specify in- and out-of-sample episodes

  target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
  tail(target_in)
  explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
  tail(explanatory_in)

  lm_obj<-lm(target_in~explanatory_in)

  summary(lm_obj)

  train<-cbind(target_in,explanatory_in)
  #-------------------------------------------

  #-------------------------------------------------------------------------------
  # Neural net fitting

  # Scaling data for the NN
  maxs <- apply(data_mat, 2, max)
  mins <- apply(data_mat, 2, min)
  # Transform data into [0,1]
  scaled <- scale(data_mat, center = mins, scale = maxs - mins)

  apply(scaled,2,min)
  apply(scaled,2,max)
  #-----------------
  # Train-test split
  # Train-test split
  train_set_xts <- scaled[paste("/",in_out_sample_separator,sep=""),]
  test_set_xts <- scaled[paste(in_out_sample_separator,"/",sep=""),]
  index(train_set_xts)

  train_set<-as.matrix(train_set_xts)
  test_set<-as.matrix(test_set_xts)

  if (use_scaled_data)
  {
    y_train<-target_in<-as.matrix(train_set[,1],ncol=1)
    x_train<-explanatory_in<-train_set[,2:ncol(train_set)]
    y_test<-target_out<-as.matrix(test_set[,1],ncol=1)
    x_test<-explanatory_out<-test_set[,2:ncol(test_set)]
  } else
  {
    y_train<-target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
    x_train<-explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
    y_test<-target_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),1]
    x_test<-explanatory_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),2:ncol(data_mat)]
  }
  return(list(x_train=x_train,x_test=x_test,y_train=y_train,y_test=y_test,train_set=train_set,test_set=test_set,target_in=target_in,explanatory_in=explanatory_in,target_out=target_out,explanatory_out=explanatory_out,data_mat=data_mat))

}






generate_quantile_LPD_adjusted_performance_func<-function(LPD_array_out_sample,k,y_test,quantile_select,length_roll_quantile,path.results,recompute_results,x_test)
{
# Use mean LPD: note that LPD has one additional dimension (in comparison to x_test,x_train) corresponding to intercept
# Therefore we use k+1 (this corresponds to explanatory x_test[,k])
  xyz<-apply(LPD_array_out_sample[,,k+1],2,mean)
  names(xyz)<-index(y_test)
  xyz<-as.xts(xyz)
  index(xyz)<-index(y_test)

  if (recompute_results)
  {
    weight_obj<-weight_trade_func(xyz,quantile_select,length_roll_quantile)
    save(weight_obj,file=paste(path.results,"/weight_obj_LPD_",100*quantile_select,"_",length_roll_quantile,sep=""))
  } else
  {
    load(file=paste(path.results,"/weight_obj_LPD_",100*quantile_select,"_",length_roll_quantile,sep=""))
  }
# Long if LPD is below upper quantile: leads to short signals during up-turns (bad)
  weight_trade_up<-weight_obj$weight_trade_up
# Long if LPD is below upper quantile and above lower quantile: short during down-turns (good) and up-turns (bad)
  weight_trade<-weight_obj$weight_trade_two_side
# Long if LPD is above lower quantile: leads to short signals during down-turns (good)
  weight_trade_low<-weight_obj$weight_trade_low
# Performance when being in long position outside critical time points identified by LPD
# Note that rolling quantile is based on lagged data (LPD is basd on x_test and is therefore lagged with respect to y_test).
  perf_weight_low<-cumsum(na.exclude(y_test*weight_trade_low))
  perf_weight_up<-cumsum(na.exclude(y_test*weight_trade_up))
# Synchronize all time series on common time span and remove NAs
  mplot_all<-na.exclude(as.matrix(cbind(cumsum(y_test),perf_weight_low,perf_weight_up,weight_trade_up,weight_trade_low,lag(xyz,k=-1))))

# We add a trading rule based on extreme (negative) values of BTC i.e. x_test[,k]: relies on lag one BTC
  if (recompute_results)
  {
    k_BTC<-1
    weight_obj<-weight_trade_func(x_test[,k_BTC],quantile_select,length_roll_quantile)
    save(weight_obj,file=paste(path.results,"/weight_obj_BTC_",100*quantile_select,"_",length_roll_quantile,sep=""))
  } else
  {
    load(file=paste(path.results,"/weight_obj_BTC_",100*quantile_select,"_",length_roll_quantile,sep=""))
  }
# Long if LPD is above lower quantile: leads to short signals during down-turns (good)
  weight_trade_low_BTC<-weight_obj$weight_trade_low
#  weight_trade_low_BTC<-weight_obj$weight_trade_up


# Performance when being in long position outside critical time points identified by LPD
  perf_weight_low_BTC<-cumsum(na.exclude(y_test*weight_trade_low_BTC))

# Synchronize all time series on common time span and remove NAs
# Rolling quantile xyz is sifted forward in order to match plots (the above trading performance is obtained woith unshifted quantile)
  mplot_all<-na.exclude(as.matrix(cbind(cumsum(y_test),perf_weight_low,perf_weight_up,perf_weight_low_BTC,weight_trade_up,weight_trade_low,lag(xyz,k=-1))))


  return(list(mplot_all=mplot_all,weight_trade_low=weight_trade_low,weight_trade_up=weight_trade_up))
}




#use_LPD_Hessian_std<-"LPD"
generate_plot_sp_func<-function(use_LPD_Hessian_std,k,LPD_array_out_sample,LPD_array_in_sample,Hessian_array_out_sample,Hessian_array_in_sample,y_test,quantile_select,length_roll_quantile,colo,sharpe_periodicity)
{
  if (use_LPD_Hessian_std=="LPD")
  {
    # Use mean LPD: note that LPD has one additional dimension (in comparison to x_test,x_train) corresponding to intercept
    # Therefore we use k+1 (this corresponds to explanatory x_test[,k])
    xyz_out<-apply(LPD_array_out_sample[,,k+1],2,mean)
    names(xyz_out)<-index(y_test)
    xyz_out<-as.xts(xyz_out)
    index(xyz_out)<-index(y_test)
    xyz_in<-apply(LPD_array_in_sample[,,k+1],2,mean)
    names(xyz_in)<-index(y_train)
    xyz_in<-as.xts(xyz_in)
    index(xyz_in)<-index(y_train)
    # In order to be able to apply rolling-windows of sufficient length (10 years) we append in-sample and out-sample LPD's
    xyz<-c(xyz_in,xyz_out)
  }

  if (use_LPD_Hessian_std=="QPD")
  {
    # Use Hessian: this has no intercept i.e. we rely in k-th element (in contrast to LPD which relies on k+1 element)
    xyz_out<-apply(Hessian_array_out_sample[,,k],2,mean)
    names(xyz_out)<-index(y_test)
    xyz_out<-as.xts(xyz_out)
    index(xyz_out)<-index(y_test)
    xyz_in<-apply(Hessian_array_in_sample[,,k],2,mean)
    names(xyz_in)<-index(y_train)
    xyz_in<-as.xts(xyz_in)
    index(xyz_in)<-index(y_train)
    # In order to be able to apply rolling-windows of sufficient length (10 years) we append in-sample and out-sample Hessians
    xyz<-c(xyz_in,xyz_out)
  }

  if (use_LPD_Hessian_std=="std")
  {
    # Use std of LPD: note that LPD has one additional dimension (in comparison to x_test,x_train) corresponding to intercept
    # Therefore we use k+1 (this corresponds to explanatory x_test[,k])
    xyz_out<-sqrt(apply(LPD_array_out_sample[,,k+1],2,var))
    names(xyz_out)<-index(y_test)
    xyz_out<-as.xts(xyz_out)
    index(xyz_out)<-index(y_test)
    xyz_in<-sqrt(apply(LPD_array_in_sample[,,k+1],2,var))
    names(xyz_in)<-index(y_train)
    xyz_in<-as.xts(xyz_in)
    index(xyz_in)<-index(y_train)
# In order to be able to apply rolling-windows of sufficient length (10 years) we append in-sample and out-sample LPD's
    xyz<-c(xyz_in,xyz_out)

  }
  tail(xyz)
  head(xyz)

  weight_obj<-weight_trade_func(xyz,quantile_select,length_roll_quantile)
  # Long if LPD is below upper quantile: leads to short signals during up-turns (bad)
  weight_trade_up<-weight_obj$weight_trade_up
  # Long if LPD is below upper quantile and above lower quantile: short during down-turns (good) and up-turns (bad)
  weight_trade_two_side<-weight_obj$weight_trade_two_side
  # Long if LPD is above lower quantile: leads to short signals during down-turns (good)
  weight_trade_low<-weight_obj$weight_trade_low
  # Performance when being in long position outside critical time points identified by LPD
  perf_weight_low<-cumsum(na.exclude(y_test*weight_trade_low))
  perf_weight_up<-cumsum(na.exclude(y_test*weight_trade_up))
  perf_weight_two_side<-cumsum(na.exclude(y_test*weight_trade_two_side))
  # Synchronize all time series on common time span and remove NAs
  mplot_all<-na.exclude(as.matrix(cbind(cumsum(y_test),perf_weight_low,perf_weight_up,perf_weight_two_side,weight_trade_up,weight_trade_low,weight_trade_two_side)))


  if (use_LPD_Hessian_std=="LPD")
  {
    # Use one-sided lower quantile: indicative of downturns
    mplot<-mplot_all[,c(1,2,3)]
    weight<-mplot_all[,ncol(mplot_all)-1]
    plot_title<-paste("Buy-and-hold (black) vs. LPD (,",colo,"): < ",100*(1-quantile_select),"%, length ",length_roll_quantile,", ",colnames(x_test)[k],sep="")
    plot_title<-paste("Buy-and-hold (black) vs. LPD (",colo,")",sep="")
    mplot<-t(t(mplot)-mplot[1,])
    sharpe_vec<-sqrt(sharpe_periodicity)*apply(apply(mplot,2,diff),2,mean)/sqrt(apply(apply(mplot,2,diff),2,var))
    colnames(mplot)<-c(paste("Buy-and-hold: Sharpe ratio ",round(sharpe_vec[1],2),sep=""),paste("LPD<",100*(1-quantile_select),"%, Sharpe ratio ",round(sharpe_vec[2],2),sep=""),paste("LPD > ",100*(quantile_select),"%",sep=""))
  }
  if (use_LPD_Hessian_std=="QPD")
  {
    # Use one-sided upper quantile: indicative of downturns
    mplot<-mplot_all[,c(1,3,3)]
    weight<-mplot_all[,ncol(mplot_all)-2]
    plot_title<-paste("Buy-and-hold (black) vs. QPD (",colo,"): > ",100*(quantile_select),"%, length ",length_roll_quantile,", ",colnames(x_test)[k],sep="")
    plot_title<-paste("Buy-and-hold (black) vs. QPD (",colo,")",sep="")
    mplot<-t(t(mplot)-mplot[1,])
    sharpe_vec<-sqrt(sharpe_periodicity)*apply(apply(mplot,2,diff),2,mean)/sqrt(apply(apply(mplot,2,diff),2,var))
    colnames(mplot)<-c(paste("Buy-and-hold: Sharpe ratio ",round(sharpe_vec[1],2),sep=""),paste("QPD>",100*(quantile_select),"%, Sharpe ratio ",round(sharpe_vec[2],2),sep=""),paste("QPD > ",100*(quantile_select),"%",sep=""))

  }
  if (use_LPD_Hessian_std=="std")
  {
# Use upper-quantile standarddeviation
    mplot<-mplot_all[,c(1,2,2)]
    weight<-mplot_all[,ncol(mplot_all)-1]
    plot_title<-paste("Buy-and-hold (black) vs. Std(LPD) (",colo,"): <",100*(1-quantile_select),"% or > ",100*(quantile_select),"%, length ",length_roll_quantile,", ",colnames(x_test)[k],sep="")
    plot_title<-paste("Buy-and-hold (black) vs. Std(LPD) (",colo,")",sep="")
    mplot<-t(t(mplot)-mplot[1,])
    sharpe_vec<-sqrt(sharpe_periodicity)*apply(apply(mplot,2,diff),2,mean)/sqrt(apply(apply(mplot,2,diff),2,var))
    colnames(mplot)<-c(paste("Buy-and-hold: Sharpe ratio ",round(sharpe_vec[1],2),sep=""),paste("Std(LPD) < ",100*(1-quantile_select),"% or >",100*(quantile_select),"%, Sharpe ratio ",round(sharpe_vec[2],2),sep=""),paste("Std(LPD) > ",100*(quantile_select),"%",sep=""))
    colnames(mplot)<-c(paste("Buy-and-hold: Sharpe ratio ",round(sharpe_vec[1],2),sep=""),paste("Std(LPD) >",100*(quantile_select),"%, Sharpe ratio ",round(sharpe_vec[2],2),sep=""),paste("Std(LPD) > ",100*(quantile_select),"%",sep=""))
  }




  return(list(mplot=mplot,plot_title=plot_title,weight=weight))
}




# This is used for toy-net in section 3: gives an alternative solution which is slightly better than neuralnet but with bogus parameters
toy_net_func<-function(x,y)
{
  # R-code: initialize
  sigmoid <- function(x){
    return(1 / (1 + exp(-x)))
  }

  # We here replicate neuralnet package
  # These are the optimal weights as optimized by neuralnet
  b1<-nn$weights[[1]][[1]][1]
  w1<-nn$weights[[1]][[1]][2]
  b2<-nn$weights[[1]][[2]][1]
  w2<-nn$weights[[1]][[2]][2]
  b1
  w1
  b2
  w2
  parm_opt_neuralnet<-c(b1,b2,w1,w2)
  names(parm_opt_neuralnet)<-c("b1","b2","w1","w2")

  # Net output: chained non-linear and linear functions: complexity!!!
  # Backcasting has to do with chain-rule of differentiation
  net_output<-function(b1,b2,w1,w2,x)
  {
    output<-sigmoid(b2+w2*sigmoid(b1+w1*x))
    return(output)
  }

  # Cost function: mean-square forecast error: should be minimized as a function of b1,w1,b2,w2
  mse_func<-function(y,output)
  {
    return(mean(abs(y-output)^2))
  }


  output<-net_output(b1,b2,w1,w2,x)

  tail(output)


  # Performance of random net with noisy initialization
  MSE_neuralnet_optimal<-mse_func(y,output)

  MSE_neuralnet_optimal
  # This one matches output of neural net...
  len*MSE_neuralnet_optimal/2
  #-------------------------------------------------
  # Arbitrary initialization
  # Initialize all parameters with 0.1
  b1<-b2<-w1<-w2<-0.1

  output<-net_output(b1,b2,w1,w2,x)

  # Performance of random net with noisy initialization
  MSE_init<-mse_func(y,output)

  # This larger than MSE_neuralnet_optimal of optimized net
  MSE_init
  # R-code: discrete step
  # Let's increase w1 a bit: try different values of delta
  delta<-0.0001
  w1_new<-w1+delta

  output_modified<-net_output(b1,b2,w1_new,w2,x)

  # Performance of random net with modified w1
  MSE_modified<-mse_func(y,output_modified)

  # MSE modified has decreased marginally: better i.e.  w1_modified is better than initial w1
  MSE_modified-MSE_init
  # Slope or approximate derivative: increase of MSE divided by delta (increment of w1)
  dw1<-(MSE_modified-MSE_init)/delta
  dw1

  # R-code: discrete gradient

  parm_init<-c(b1,b2,w1,w2)


  # This is a discrete proxy of gradient: perturbation applied to all parameters
  gradient_func<-function(parm,x,y,delta)
  {

    parm_modified<-parm
    b1<-parm[1]
    b2<-parm[2]
    w1<-parm[3]
    w2<-parm[4]
    output<-net_output(b1,b2,w1,w2,x)
    MSE<-mse_func(y,output)

    gradient<-rep(NA,length(parm))
    for (i in 1:length(parm))
    {
      parm_modified<-parm
      parm_modified[i]<-parm[i]+delta
      b1<-parm_modified[1]
      b2<-parm_modified[2]
      w1<-parm_modified[3]
      w2<-parm_modified[4]
      output_modified<-net_output(b1,b2,w1,w2,x)
      MSE_modified<-mse_func(y,output_modified)
      gradient[i]<-(MSE_modified-MSE)/delta
    }
    names(gradient)<-c("b1","b2","w1","w2")
    return(gradient)
  }

  gradient<-gradient_func(parm_init,x,y,delta)
  gradient
  # Check with above result for w1
  (MSE_modified-MSE_init)/delta



  # up-date the parameters: go in negative direction of gradient; scale with learn-rate
  # Large learn-rate: big up-dating step (learn rapidly and 'shoot over target')
  # Small learn-rate: small up-dating step (learn progressively/slowly/not at all...)
  learn_rate<-10
  learn_rate<-0.1
  parm_up<-parm_init-learn_rate*gradient

  b1<-parm_up[1]
  b2<-parm_up[2]
  w1<-parm_up[3]
  w2<-parm_up[4]

  output_up<-net_output(b1,b2,w1,w2,x)

  # Performance of random net with up dated parameter vector
  MSE_up<-mse_func(y,output_up)

  # Improvement: depends on learn-rate
  MSE_up-MSE_init

  # This function accepts the parameter and the data and returns MSE
  # Can be used for numerical optimization
  optimize_toy_net<-function(parm,x,y)
  {
    b1<-parm[1]
    b2<-parm[2]
    w1<-parm[3]
    w2<-parm[4]

    output<-net_output(b1,b2,w1,w2,x)

    MSE<-mse_func(y,output)

    return(MSE)
  }

  nlmin_obj<-nlminb(parm_init,optimize_toy_net,x=x,y=y)

  par_optim<-nlmin_obj$par
  names(par_optim)<-c("b1","b2","w1","w2")
  par_optim
  # Result from optimization
  opt_mse<-nlmin_obj$objective
  opt_mse
  # Confirmation of forecast MSE-performance of optimized parameter
  optimize_toy_net(par_optim,x,y)
  # Compare with neuralnet: not as good as our own...
  MSE_neuralnet_optimal
  # Compare estimates....: crazy different: XAI (want to understand model)
  parm_opt_neuralnet
  par_optim

  # The gradient should vanish at optimum: check
  #   But this is not the exact gradient!!! See below
  gradient_func(par_optim,x,y,delta)
  return(list(nlmin_obj=nlmin_obj,par_optim=par_optim))
}
