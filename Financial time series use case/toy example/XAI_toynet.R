
rm(list=ls())

path.main<-getwd()

path.dat<-path.main

# Load  relevant packages

inst_pack<-rownames(installed.packages())

if (!"xts"%in%inst_pack)
  install.packages("xts")


library(xts)
#library(xtable)

source(paste(getwd(),"/neuralnet_functions.R",sep=""))


# Load data
load(paste(path.dat,"/bitoin_toy_data.Rdata",sep=""))

x_train<-data_list$x_train
y_train<-data_list$y_train





# Specify toy-net


neuron_vec<-1
# Output: non-linear (linear_output<-F) or linear (linear_output<-T)
linear_output<-F
# Activation function: sigmoid (atan_not_sigmoid<-F) or atan (atan_not_sigmoid<-T)
atan_not_sigmoid<-F

hidden_neurons<-neuron_vec

list_layer_size<-layer_size<-getLayerSize(x_train, y_train, hidden_neurons)




# Fit parameters
learning_rate<-0.3
epochs<-50
#--
if (linear_output)
  learning_rate<-learning_rate/10
#--
hyper_list<-list(epochs=epochs,learning_rate=learning_rate,linear_output=linear_output,atan_not_sigmoid=atan_not_sigmoid,neuron_vec=neuron_vec)

# Train/Learn/Optimize
train_model <- trainModel(x_train, y_train, hyper_list=hyper_list)

# Fitting progress
train_model$cost_hist

#------------------------------------------------------------------
# LPD
updated_params<-train_model$updated_params
fwd_prop <- forwardPropagation(x_train, updated_params, layer_size,linear_output,atan_not_sigmoid)
cache<-fwd_prop
output<-fwd_prop$A_list[[length(fwd_prop$A_list)]]
# MSE of optimized net
cost <- computeCost(y_train, fwd_prop)
cost

LPD_obj<-LPD( cache, updated_params, list_layer_size,linear_output,atan_not_sigmoid)

in_sample_LPD_t<-LPD_obj$LPD_t

ts.plot(in_sample_LPD_t)

#----------------------------------------------------------------
# Generate 100 random samples of LPD

set.seed(1)
anzsim<-100
LPD_mat<-NULL
for (i in 1:anzsim)
{
  train_model <- trainModel(x_train, y_train, hyper_list=hyper_list)
# LPD
  updated_params<-train_model$updated_params
  fwd_prop <- forwardPropagation(x_train, updated_params, layer_size,linear_output,atan_not_sigmoid)
  cache<-fwd_prop
  LPD_obj<-LPD( cache, updated_params, list_layer_size,linear_output,atan_not_sigmoid)
  LPD_mat<-cbind(LPD_mat,LPD_obj$LPD_t)
  
}

ts.plot(LPD_mat)

mean_LPD<-apply(LPD_mat,1,mean)
sd_LPD<-apply(LPD_mat,1,sd)


ts.plot(LPD_mat,ylim=c(min(cbind(LPD_mat,mean_LPD-2*sd_LPD)),max(cbind(LPD_mat,mean_LPD+2*sd_LPD))),main="Random LPDs, mean-LPD and 2-sigma band")
lines(mean_LPD,col="blue",lwd=3)
lines(mean_LPD+2*sd_LPD,col="red",lwd=3)
lines(mean_LPD-2*sd_LPD,col="red",lwd=3)

