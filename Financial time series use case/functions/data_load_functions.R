
#hour_of_day<-"16:00"

data_load_func<-function(asset_vec_IB,series_i_vec,hour_of_day,path.dat)
{
  
  tz_char<-'CET'
  Sys.setenv(TZ=tz_char)
  Sys.timezone()
  load(paste(path.dat,"data_list",sep=""))#length(data_list)  names(data_list)
  # Extract time slot from intraday data    series_i_vec<-1:16
  extract_time_slot.obj<-extract_time_slot(data_list,hour_of_day,series_i_vec,asset_vec_IB)
  # Remove NAs previous to computing differences (otherwise Mondays are NA)  
  FX_mat_bid<-na.exclude(extract_time_slot.obj$FX_mat_bid)
  FX_mat_ask<-na.exclude(extract_time_slot.obj$FX_mat_ask)
  tail(FX_mat_ask)
  tail(FX_mat_bid)
  FX_mat_mean<-(FX_mat_ask+FX_mat_bid)/2
  tail(FX_mat_mean)
  
  FX_diff<-diff(FX_mat_mean)#tail(FX_diff)
  return(list(FX_mat_ask=FX_mat_ask,FX_mat_bid=FX_mat_bid,FX_diff=FX_diff,FX_mat_mean=FX_mat_mean))
}


extract_time_slot<-function(data_list,hour_of_day,series_i_vec,asset_vec_IB)
{
  for (series_i in 1:length(series_i_vec))#series_i<-length(series_i_vec)  hour_of_day<-"16:00"
  {
    if (series_i==1)
    {
      FX_mat_bid<-as.xts(data_list[[asset_vec_IB[series_i_vec[series_i]]]]$price_bid[,hour_of_day])#names(data_list)[[series_i_vec[series_i]]])
      FX_mat_ask<-as.xts(data_list[[asset_vec_IB[series_i_vec[series_i]]]]$price_ask[,hour_of_day])
    } else
    {
      FX_mat_bid<-cbind(FX_mat_bid,as.xts(data_list[[asset_vec_IB[series_i_vec[series_i]]]]$price_bid[,hour_of_day]))
      FX_mat_ask<-cbind(FX_mat_ask,as.xts(data_list[[asset_vec_IB[series_i_vec[series_i]]]]$price_ask[,hour_of_day]))
    }
  }
  colnames(FX_mat_ask)<-colnames(FX_mat_bid)<-asset_vec_IB[series_i_vec]#dim(FX_mat_ask)
  tail(FX_mat_bid)
  tail(FX_mat_ask,100)
  return(list(FX_mat_bid=FX_mat_bid,FX_mat_ask=FX_mat_ask))
} 



data_load_gzd_trading_func<-function(data_from_IB,hour_of_day,reload_sp500,path.dat)
{  
  
  if (data_from_IB)
  {
    asset_vec_IB<-c("EURUSD","EURJPY","GBPJPY","CHFJPY","USDCHF","GBPUSD","USDJPY","EURGBP","EURCHF","GBPCHF")
    
    series_i_vec<-1:length(asset_vec_IB)
    
    data_load.obj<-data_load_func(asset_vec_IB,series_i_vec,hour_of_day,path.dat)
    
    x<-na.exclude(data_load.obj$FX_diff)#tail(x)
    price<-na.exclude(data_load.obj$FX_mat_mean)#index(price)
    xts_data_mat<-na.exclude(price)
    colnames(xts_data_mat)
    head(xts_data_mat)
    tail(xts_data_mat)
    
  } else
  {
    currencies<-read.csv(file="data/Currencies_2.csv")
    tail(currencies)
    head(currencies)
    xts_data_mat <- xts::as.xts(currencies[,2:ncol(currencies)],
                                order.by = as.POSIXct(currencies[,1],
                                                      format = "%m/%d/%Y",
                                                      tz = "CET"))
    xts_data_mat<-na.exclude(xts_data_mat)
    dim(xts_data_mat)
    head(xts_data_mat)
    tail(xts_data_mat)
    
  }
  
  # S&P500: for application towards a series with marked trend
  if (reload_sp500)
  {
    library(Quandl)
    end_date<-format(Sys.time(), "%Y-%m-%d")
    start_date<-'1990-01-01'
    # S&P500
    # we first load s&p in order to have the full time index (if GDP is loaded first then the time index is based on quarters...)    
    ser_load<-Quandl("CHRIS/CME_SP1",start_date=start_date,end_date=end_date,type='xts')
    mydata<-as.xts(na.omit(ser_load[,"Last"]))#tail(ser_load)  which(is.na(ser_load[,"Last"]))
    mydata<-as.xts(na.locf(ser_load[,"Last"]))#tail(ser_load)  which(is.na(ser_load[,"Last"]))
    save(mydata,file=paste(path.dat,"sp500.Rdata",sep=""))
  } else
  {
    load(file=paste(path.dat,"sp500.Rdata",sep=""))
  }
  return(list(mydata=mydata,xts_data_mat=xts_data_mat))
}
#---------------------------------------------------------------------------------------------------------------

