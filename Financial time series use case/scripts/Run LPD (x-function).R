# x-function for time series 

rm(list=ls())


# Packages that need to be installed and called 
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


# Load Bitcoin data
# Set wd where bitcoin.Rdata is
# setwd("~/GitHub/XAI_Marc 2")
path.dat = getwd()
load(paste(path.dat,"/Data/bitcoin.Rdata",sep=""))


# Descriptive 
head(dat)
tail(dat)


# plot last, bid and ask in single figure names(dat)
par(mfrow=c(2,2))
plot(dat$Bid,col=1,main="Prices")
plot(log(dat$Bid),col=1,on=1,main="Log-prices")  #tail(dat$Bid)
plot(diff(log(dat$Bid)),col=1,on=1,main="Log-returns")
plot(log(dat$Volume),col=1,on=1,main="Log-volumes")

# Specify target and explanatory data: we use first six lags based on above data analysis
x = na.omit(diff(log(dat$Bid)))
data_mat = cbind(x,lag(x),lag(x,k=2),lag(x,k=3),lag(x,k=4),lag(x,k=5),lag(x,k=6))
dim(data_mat)
data_mat = na.exclude(data_mat)
dim(data_mat)
head(data_mat)
tail(data_mat)


# Specify in- and out-of-sample episodes
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

# Neural net fitting
# Scaling data for the NN
maxs <- apply(data_mat, 2, max)
mins <- apply(data_mat, 2, min)


# Transform data into [0,1] and check transformation 
scaled <- scale(data_mat, center = mins, scale = maxs - mins)
apply(scaled,2,min)
apply(scaled,2,max)


# Train-test split
train_set <- scaled[paste("/",in_out_sample_separator,sep=""),]
test_set <- scaled[paste(in_out_sample_separator,"/",sep=""),]
train_set<-as.matrix(train_set)
test_set<-as.matrix(test_set)
colnames(train_set)<-paste("lag",0:(ncol(train_set)-1),sep="")
n <- colnames(train_set)


# Model: target is current bitcoin, all other variables are explanatory
f <- as.formula(paste("lag0 ~", paste(n[!n %in% "lag0"], collapse = " + ")))

# Set/fix the random seed and fit the nn using the neuralnet function 
set.seed(1)
nn <- neuralnet(f,data=train_set,hidden=c(3,2),linear.output=F)
plot(nn)

# Performance 
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



# X-function: Linear Parameter Data 
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



# Apply the X-function 
# Original model: nn
nn

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


# We train the net for 100 different random initialization and we observe the dependency of the LPDs across the different random nets.
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

# Check mean LPDs
dim(data)
dim(OLPD_mat_mean)
rownames(OLPD_mat_mean)<-rownames(data)
OLPD_mat_mean<-as.xts(OLPD_mat_mean)
colo<-rainbow(ncol(OLPD_mat_mean))

# Plot
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


# Select a particular explanatory
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






