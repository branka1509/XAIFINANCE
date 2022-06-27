# Train Classic XAI on Financial Time Series Data
rm(list=ls())


# Libraries that have to be installed and called
if (F)
{
  install.packages("h2o")
  install.packages("xts")
  install.packages("shapper")
  install.packages("neuralnet")
  install.packages("DALEX")
}

library(h2o)
library(xts)
library(shapper)
library(neuralnet)
library(DALEX)

# Initiated h2o
h2o.init()
install_shap()

# Load Bitcoin data
# Set wd where bitcoin.Rdata is
# setwd("~/GitHub/XAI_Marc 2")
path.dat = getwd()
load(paste(path.dat,"/Data/bitcoin.Rdata",sep=""))


# Descriptive
head(dat)
tail(dat)

# plot last, bid and ask in single figure names(dat)
par(mfrow =c(2,2))
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


# Set/fix the random seed
set.seed(1)

# Create h2o environments for the train and test set
test = as.h2o(test_set)
train = as.h2o(train_set)


# Build and train the model using the h2o package 
# Model: target is current bitcoin, all other variables are explanatory
dl = h2o.deeplearning(x = 2:7,
                       y = "lag0",
                       hidden = c(3,2),
                       epochs = 1000,
                       seed = 1,
                       training_frame = train)

dl

# Eval performance
perf = h2o.performance(dl)
perf


# Generate predictions on a test set (if necessary):
pred = h2o.predict(dl, newdata = test)
pred

eval = as.data.frame(pred)
eval$real = test_set$lag0


# Explain the model using classic XAI
# Explain a model
classic = h2o.explain(dl, test)
classic[["residual_analysis"]] # residual analysis 
classic[["varimp"]]            # variable importance
classic[["pdp"]]               # pdp
classic[["ice"]]               # ice


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
exp_nn_shap
plot(exp_nn_shap)

