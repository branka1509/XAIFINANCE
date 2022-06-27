# Topic: regression vs. simple feedforward based on nn
#   Application to bitcoin
# First week

rm(list=ls())


# Die folgenden packages muss man beim ersten Mal installieren (falls noch nicht installiert...)
if (F)
{
  install.packages("neuralnet")
  install.packages("fGarch")
  install.packages("xts")
  install.packages("Quandl")
}
library(neuralnet)
library(fGarch)
library(xts)
library(Quandl)



###################################################################
# Exercise 1: Load Bitcoin data
# 1.a
Quandl.api_key("yUtHvNxMBzGmnxKbQ79z")
load_data<-F
path.dat<-getwd()
#path.dat<-paste(path.main,"/Exercises/Erste Woche/Data/",sep="")


if (load_data)
{
  dat=Quandl(code = "BITSTAMP/USD",type="xts")
  summary(dat)
  tail(dat$Bid)
  print(path.dat)
  #save(dat,file=paste(path.dat,"bitcoin.Rdata",sep=""))
} else
{
  load(paste(path.dat,"/Data/bitcoin.Rdata",sep=""))
}
#-------------------
# 1.b
head(dat)
tail(dat)
#-------------------
# 1.c Plot  data


# plot last, bid and ask in single figure names(dat)
par(mfrow=c(2,2))
plot(dat$Bid,col=1,main="Prices")
plot(log(dat$Bid),col=1,on=1,main="Log-prices")  #tail(dat$Bid)
plot(diff(log(dat$Bid)),col=1,on=1,main="Log-returns")
plot(log(dat$Volume),col=1,on=1,main="Log-volumes")


#-----------------------------
# 1.d
# Evidence: vola-clustering

########################################################################################-----------------------------
# Fit a GARCH to log-returns
# 2.a

x_fit<-as.ts(na.omit(diff(log(dat$Bid))))
# GARCH(1,1)
y.garch_11<-garchFit(~garch(1,1),data=x_fit,delta=2,include.delta=F,include.mean=F,trace=F)

ts.plot(x_fit)
lines(y.garch_11@sigma.t,col="red")
#-----------------
# 2.b
standard_residuals<-y.garch_11@residuals/y.garch_11@sigma.t
ts.plot(standard_residuals)
#-----------------
# 2.c
par(mfrow=c(2,1))
acf(x_fit,main="Acf log-returns",ylim=c(0,0.1))
acf(standard_residuals,main="Acf standardized residuals GARCH(1,1)",ylim=c(0,0.1))

####################################################################
# Classic linear regression: applied to Bid-prices
# Specify target and explanatory data: we use first six lags based on above data analysis
# 3.2
x<-na.omit(diff(log(dat$Bid)))
#------------------
# 3.b
data_mat<-cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6))
# Check length of time series before na.exclude
dim(data_mat)
data_mat<-na.exclude(data_mat)
# Check length of time series after removal of NAs
dim(data_mat)
head(data_mat)
tail(data_mat)

#--------------------------------------------------------------------
# 3.c&d Specify in- and out-of-sample episodes
in_out_sample_separator<-"2015-06-01"

target_in<-data_mat[paste("/",in_out_sample_separator,sep=""),1]
tail(target_in)
explanatory_in<-data_mat[paste("/",in_out_sample_separator,sep=""),2:ncol(data_mat)]
tail(explanatory_in)

target_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),1]
head(target_out)
tail(target_out)
explanatory_out<-data_mat[paste(in_out_sample_separator,"/",sep=""),2:ncol(data_mat)]
head(target_out)
tail(explanatory_out)

train<-cbind(target_in,explanatory_in)
test<-cbind(target_out,explanatory_out)
head(test)
tail(test)
nrow(test)
#-------------------------------------------
# 3.e&f
# Fitting linear model to log-returns (could be applied to standardized log-returns: returns divided by vola)
lm.fit <- lm(target_in~explanatory_in)#mean(target_in)plot(cumsum(target_in))plot(log(dat$Bid))
summary(lm.fit)
# Without intercept
lm.fit <- lm(target_in~explanatory_in-1)
summary(lm.fit)
#----------------------------------------------
# 3.g
# Predicted data from lm
#   Without intercept
predicted_lm<-explanatory_out%*%lm.fit$coef

# Same as above single line of code (but more explicit)
for (i in 1:nrow(explanatory_out))
{
  predicted_lm[i,]<-sum(explanatory_out[i,]*lm.fit$coef)
}


# With intercept: we have to add a column of 1s to the explanatory data (for the additional intercept)
if (length(lm.fit$coef)>ncol(explanatory_out))
  predicted_lm<-cbind(rep(1,nrow(explanatory_out)),explanatory_out)%*%lm.fit$coef

#------------------------------------------
# 3.h
# Test MSE: in-sample vs. out-of-sample
MSE.in.lm<-mean(lm.fit$residuals^2)
MSE.out.lm <- sum((predicted_lm - target_out)^2)/nrow(test)
c(MSE.in.lm,MSE.out.lm)
#--------------------------------
# 3.i Trading performance
perf_lm<-(sign(predicted_lm))*target_out


sharpe_lm<-sqrt(365)*mean(perf_lm,na.rm=T)/sqrt(var(perf_lm,na.rm=T))

plot(cumsum(perf_lm),main=paste("Linear regression cumulated performances out-of-sample, sharpe=",round(sharpe_lm,2),sep=""))

#-------------------------------------------------------------------------------
# 4. Neural net fitting

# 4.a
# Scaling data for the NN
maxs <- apply(data_mat, 2, max)
mins <- apply(data_mat, 2, min)
# Transform data into [0,1]
scaled <- scale(data_mat, center = mins, scale = maxs - mins)

apply(scaled,2,min)
apply(scaled,2,max)
#-----------------
# 4.b
# Train-test split
train_set <- scaled[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled[paste(in_out_sample_separator,"/",sep=""),]

train_set<-as.matrix(train_set)
test_set<-as.matrix(test_set)
#-----------------------------------
# 4.c

colnames(train_set)<-paste("lag",0:(ncol(train_set)-1),sep="")
n <- colnames(train_set)
# Model: target is current bitcoin, all other variables are explanatory
f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))

# Set/fix the random seed
set.seed(1)
nn <- neuralnet(f,data=train_set,hidden=c(3,2),linear.output=F)
#------------------------------------
# 4.d (compare different realizations of the above net: train the net without specifying set.seed)
plot(nn)

#----------------------------------------------
# 4.e
# In sample performance
MSE.in.nn<-mean(((train_set[,1]-nn$net.result[[1]])*(max(data_mat[,1])-min(data_mat[,1])))^2)

# Out-of-sample performance
pr.nn <- compute(nn,test_set[,2:ncol(test_set)])

predicted_scaled<-pr.nn$net.result
# Results from NN are normalized (scaled)
# Descaling for comparison
predicted_nn <- predicted_scaled*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
test.r <- test_set[,1]*(max(data_mat[,1])-min(data_mat[,1]))+min(data_mat[,1])
# Calculating MSE
MSE.out.nn <- sum((test.r - predicted_nn)^2)/nrow(test_set)

# Compare in-sample and out-of-sample
c(MSE.in.nn,MSE.out.nn)

# Compare Regression and nn in-sample: which model would you prefer/select?
print(paste(MSE.in.lm,MSE.in.nn))
# Compare Regression and nn in-sample: which model was better
print(paste(MSE.out.lm,MSE.out.nn))

#------------------------------------------------------------------------------------------------------------------




# Original linear parameter data
#   Generate new data from original data
#   New data: in each time point compute the parameters of the exact infinitesimal linear regression model
OLPD_func<-function(x,delta,epsilon,nn)
{
  out_original<-compute(nn,x)$net.result
  # For each explanatory...
  for (i in 1:ncol(x))#i<-1
  {
    # y will be the original explanatory plus an infinitesimal perturbation of i-th explanatory
    y<-x
    y[,i]<-y[,i]+delta*x[,i]

    # Generate infinitesimally perturbated output
    out_i <-compute(nn,y)$net.result

    if (i==1)
    {
      effect<-(out_i-out_original)/(delta*x[,i])
    } else
    {
      effect<-c(effect,(out_i-out_original)/(delta*x[,i]))
    }
    # Collect for each explanatory the perturbated data and the corresponding nn-output
    #    }
  }
  # Fit the regression to the noiseless perturbated data: as many observations as unknowns i.e. zero-residual
  return(list(effect=effect))
}








set.seed(1)

neuron_vec<-c(3,2)

nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=F)

#plot(nn)


# Induce infinitesimal perturbations to data and fit regression to output

delta<-1.e-5
epsilon<-1.e-4

pb <- txtProgressBar(min = 1, max = (nrow(train_set)-1), style = 3)

# Rougher (out-sample data)
data<-test_set
# Smoother (in-sample data)
data<-train_set


for (i in 1:(nrow(data)))
{
  x<-matrix(data[i,2:ncol(data)],nrow=1)
  colnames(x)<-colnames(data)[2:ncol(data)]
  OLPD_obj<-OLPD_func(x,delta,epsilon,nn)

  if (i==1)
  {
    OLPD_mat<-OLPD_obj$effect
  } else
  {
    OLPD_mat<-rbind(OLPD_mat,OLPD_obj$effect)
  }
  setTxtProgressBar(pb, i)

}
close(pb)

rownames(OLPD_mat)<-rownames(data)
OLPD_mat<-as.xts(OLPD_mat)

par(mfrow=c(2,1))
plot(OLPD_mat,col=rainbow(ncol(OLPD_mat)))
plot(cumsum(target_in))
#--------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------


asset_sel<-"bitcoin"

set.seed(1)
# Net architecture
neuron_vec<-c(3,2)
neuron_name<-NULL
for (i in 1:length(neuron_vec))
  neuron_name<-paste(neuron_name,"_",neuron_vec[i],sep="")
# Number of random nets
anz_real<-100
# Evaluate net in-sample
data<-train_set
# Evaluate net out-of-sample
data<-test_set
data_xts<-as.xts(data)

# Array of sensitivities
arr_sens<-array(dim=c(anz_real,nrow(data),ncol(data)-1))
pb <- txtProgressBar(min = 1, max = anz_real, style = 3)

compute_results<-F

if (compute_results)
{
  # Loop over all random-nets
  for (j in 1:anz_real)#j<-1
  {

    nn <- neuralnet(f,data=train_set,hidden=neuron_vec,linear.output=F)

    # Rougher (out-sample data)
    # Smoother (in-sample data)
    # data<-train_set


    for (i in 1:(nrow(data)))
    {
      x<-matrix(data[i,2:ncol(data)],nrow=1)
      colnames(x)<-colnames(data)[2:ncol(data)]

      OLPD_obj<-OLPD_func(x,delta,epsilon,nn)

      if (i==1)
      {
        OLPD_mat<-OLPD_obj$effect
      } else
      {
        OLPD_mat<-rbind(OLPD_mat,OLPD_obj$effect)
      }

    }
    arr_sens[j,,]<-OLPD_mat
    setTxtProgressBar(pb, j)
  }
  close(pb)

  # Compute aggregate/mean sensitivities


  OLPD_mat_mean<-apply(arr_sens,c(2,3),mean)
  OLPD_mat_var<-apply(arr_sens,c(2,3),var)

  save(OLPD_mat_mean,file=paste(getwd(),"/Output/OLPD_mat_mean_",asset_sel,neuron_name,sep=""))
  save(OLPD_mat_var,file=paste(getwd(),"/Output/OLPD_mat_var_",asset_sel,neuron_name,sep=""))

} else
{
  load(file=paste(getwd(),"/Output/OLPD_mat_mean_",asset_sel,neuron_name,sep=""))
  load(file=paste(getwd(),"/Output/OLPD_mat_var_",asset_sel,neuron_name,sep=""))

}

dim(data)
dim(OLPD_mat_mean)
rownames(OLPD_mat_mean)<-rownames(data)
OLPD_mat_mean<-as.xts(OLPD_mat_mean)
colo<-rainbow(ncol(OLPD_mat_mean))
par(mfrow=c(2,1))
plot(OLPD_mat_mean,ylim=c(-1,1),main="Mean sensitivities: partial derivative",col=colo)
for (i in 1:ncol(OLPD_mat_mean))
  mtext(paste("Lag ",i,sep=""),col=colo[i],line=-i)
lbit<-log(dat$Bid)[paste(index(OLPD_mat_mean)[1],"/",sep="")]
lbit<-lbit[paste("/",index(OLPD_mat_mean)[nrow(OLPD_mat_mean)],sep="")]
plot(lbit,col=1,on=1,main="Log-Bitcoin")  #tail(dat$Bid)

par(mfrow=c(1,1))
plot(OLPD_mat_mean,ylim=c(-1,1),main="Mean sensitivities: partial derivative",col=colo)
for (i in 1:ncol(OLPD_mat_mean))
  mtext(paste("Lag ",i,sep=""),col=colo[i],line=-i)





OLPD_mat_lower<-OLPD_mat_mean-2.2*sqrt(OLPD_mat_var)#/sqrt(anz_real)
OLPD_mat_upper<-OLPD_mat_mean+2.2*sqrt(OLPD_mat_var)#/sqrt(anz_real)

rownames(OLPD_mat_lower)<-rownames(OLPD_mat_upper)<-rownames(data)
OLPD_mat_lower<-as.xts(OLPD_mat_lower)
OLPD_mat_upper<-as.xts(OLPD_mat_upper)
colo<-rainbow(ncol(OLPD_mat_lower))
par(mfrow=c(2,2))
# select a particular explanatory
select_exp<-6
mplot<-cbind(OLPD_mat_mean[,select_exp],OLPD_mat_lower[,select_exp],OLPD_mat_upper[,select_exp])
plot(mplot,ylim=c(-1,1),main=paste("Confidence interval for sensitivity of ",colnames(data)[select_exp+1],sep=""),col=rep(colo[select_exp],3),lty=c(1,2,2))
select_exp<-2
mplot<-cbind(OLPD_mat_mean[,select_exp],OLPD_mat_lower[,select_exp],OLPD_mat_upper[,select_exp])
plot(mplot,ylim=c(-1,1),main=paste("Confidence interval for sensitivity of ",colnames(data)[select_exp+1],sep=""),col=rep(colo[select_exp],3),lty=c(1,2,2))
select_exp<-4
mplot<-cbind(OLPD_mat_mean[,select_exp],OLPD_mat_lower[,select_exp],OLPD_mat_upper[,select_exp])
plot(mplot,ylim=c(-1,1),main=paste("Confidence interval for sensitivity of ",colnames(data)[select_exp+1],sep=""),col=rep(colo[select_exp],3),lty=c(1,2,2))
select_exp<-5
mplot<-cbind(OLPD_mat_mean[,select_exp],OLPD_mat_lower[,select_exp],OLPD_mat_upper[,select_exp])
plot(mplot,ylim=c(-1,1),main=paste("Confidence interval for sensitivity of ",colnames(data)[select_exp+1],sep=""),col=rep(colo[select_exp],3),lty=c(1,2,2))






