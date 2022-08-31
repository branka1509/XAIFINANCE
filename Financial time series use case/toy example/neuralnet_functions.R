


getLayerSize <- function(x_train, y, hidden_neurons) {
  n_x <- dim(x_train)[2]
  n_h <- hidden_neurons
  n_y <- dim(y)[2]

  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)

  return(size)
}




initializeParameters <- function(list_layer_size){


  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  W_list <- vector(mode="list")
  b_list <- vector(mode="list")
  W_list[[1]]<-matrix(runif(n_h[1] * n_x), nrow = n_h[1], ncol = n_x, byrow = TRUE) * 0.9/n_x
  b_list[[1]]<-matrix(rep(0, n_h[1]), nrow = n_h[1])
  i<-1
  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))
    {
      W_list[[i]]<-matrix(runif(n_h[i] * n_h[i-1]), nrow = n_h[i], ncol = n_h[i-1], byrow = TRUE)*0.9/n_h[i-1]
      b_list[[i]]<-matrix(rep(0, n_h[i]), nrow = n_h[i])

    }
  }
  W_list[[i+1]]<-matrix(runif(n_y * n_h[length(n_h)]), nrow = n_y, ncol = n_h[length(n_h)], byrow = TRUE) * 0.9/n_y
  b_list[[i+1]]<- matrix(rep(0, n_y), nrow = n_y)


  params <- list("W_list" = W_list,"b_list"=b_list)

  return (params)
}



sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}


# params<-init_params
# params<-parm
forwardPropagation <- function(x_train, params, list_layer_size,linear_output,atan_not_sigmoid)
{

  m <- dim(x_train)[1]
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  Z_list <- vector(mode="list")
  A_list <- vector(mode="list")
# Input layer to first hidden
  W <- params$W_list[[1]]
  b <- params$b_list[[1]]
  b_new <- matrix(rep(b, m), nrow = n_h[1])
# Inputs of neurons in first hidden layer
  Z_list[[1]] <- W %*% t(x_train) + b_new
# Outputs of first hidden layer
  if (atan_not_sigmoid)
  {
    A_list[[1]] <- atan(Z_list[[1]])
  } else
  {
    A_list[[1]] <- sigmoid(Z_list[[1]])
  }

# From hidden to hidden
  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))
    {
      W <- params$W_list[[i]]
      b <- params$b_list[[i]]
      b_new <- matrix(rep(b, m), nrow = n_h[i])
      # Inputs of neurons in hidden layer
#      Z_list[[i]] <- W %*% Z_list[[i-1]]  + b_new
      Z_list[[i]] <- W %*% A_list[[i-1]]  + b_new
      # Outputs of hidden layer
      if (atan_not_sigmoid)
      {
        A_list[[i]] <- atan(Z_list[[i]])
      } else
      {
        A_list[[i]] <- sigmoid(Z_list[[i]])
      }
    }
  } else
  {
    i<-1
  }

# Output neuron
  W <- params$W_list[[i+1]]
  b <- params$b_list[[i+1]]
  b_new <- matrix(rep(b, m), nrow = n_y)

# Inputs of output neuron
  Z_list[[i+1]] <- W %*% A_list[[i]] + b_new
  # Output of neural net
  if (linear_output)
  {
    A_list[[i+1]] <- Z_list[[i+1]]
  } else
  {
    if (atan_not_sigmoid)
    {
      A_list[[i+1]] <- atan(Z_list[[i+1]])
    } else
    {
      A_list[[i+1]] <- sigmoid(Z_list[[i+1]])
    }
  }

  cache <- list("Z_list" = Z_list,"A_list" = A_list)
  return (cache)
}







# https://rviews.rstudio.com/2020/07/24/building-a-neural-net-from-scratch-using-r-part-2/
computeCost <- function(y_train, cache) {
  A_out <- cache$A_list[[length(cache$A_list)]]
  forecast_error<-as.vector(y_train)-A_out
  cost <- mean(forecast_error^2)
  return (cost)
}


# Backpropagation
# Derivative of cost (MSE) with respect to parameters (not input data)
backwardPropagation <- function(x_train, y_train, cache, params, list_layer_size,linear_output,atan_not_sigmoid)
{
  #params<-parm
  #y<-y_train
  #cache<-fwd_prop


  m <- dim(x_train)[1]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  dA_list<-dA_listh <- vector(mode="list")
  dW_list <-dW_listh<- vector(mode="list")
  db_list <-db_listh<- vector(mode="list")

# derivative of MSE at output: derivative of atan(x) is 1/(1+x^2)
  if (linear_output)
  {
    dA_listh[[1]]<-2*(cache$A_list[[length(cache$A_list)]]-as.vector(y_train))
  } else
  {
    if (atan_not_sigmoid)
    {
      dA_listh[[1]]<-2*(cache$A_list[[length(cache$A_list)]]-as.vector(y_train))/(1+cache$Z_list[[length(cache$Z_list)]]^2)
    } else
    {
      dA_listh[[1]]<-2*(cache$A_list[[length(cache$A_list)]]-as.vector(y_train))*(1-cache$A_list[[length(cache$A_list)]])*cache$A_list[[length(cache$A_list)]]
    }
  }
  dW_listh[[1]]<-1/m * ( dA_listh[[1]] %*% t(cache$A_list[[length(cache$A_list)-1]]))
  db_listh[[1]]<-matrix(1/m * sum( dA_listh[[1]]), nrow = n_y)
  dim(dW_listh[[1]])
  dim( db_listh[[1]])
  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))#i<-2
    {
# Runs backwards i.e. ordering of n_h is reversed
      if (atan_not_sigmoid)
      {
        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])/(1+cache$Z_list[[length(n_h)+2-i]]^2)
      } else
      {
        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])*(1-cache$A_list[[length(n_h)+2-i]])*cache$A_list[[length(n_h)+2-i]]
      }
      dW_listh[[i]]<-1/m * (dA_listh[[i]] %*% t(cache$A_list[[length(cache$A_list)-i]]))
      db_listh[[i]]<-matrix(1/m * apply(dA_listh[[i]],1,sum), nrow = n_h[length(n_h)+2-i])
    }
  } else
  {
    i<-1
  }
# Input layer
  if (atan_not_sigmoid)
  {
    dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])/(1+cache$Z_list[[1]]^2)
  } else
  {
    dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])*(1-cache$A_list[[1]])*cache$A_list[[1]]
  }
  dW_listh[[i+1]]<-1/m * (dA_listh[[i+1]]%*% (x_train))
  db_listh[[i+1]]<-matrix(1/m * apply(dA_listh[[i+1]],1,sum), nrow = n_h[1])
# revert ordering: from left to right
  for (i in 1:length(dA_listh))
  {
    dA_list[[i]]<-dA_listh[[(1+length(dA_listh))-i]]
    dW_list[[i]]<-dW_listh[[(1+length(dW_listh))-i]]
    db_list[[i]]<-db_listh[[(1+length(db_listh))-i]]
  }
  grads <- list("dW_list" = dW_list,"db_list" = db_list)

  return(grads)
}



# Derivative of output (not cost/MSE) with respect to input data (not parameters)
# This is based on backpropagation (which is a bit awkward and useless when computing the Hessian)
LPD <- function(cache, params, list_layer_size,linear_output,atan_not_sigmoid)
{
  #params<-parm


  m <- dim(cache$A_list[[length(cache$A_list)]])[2]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  dA_list<-dA_listh <- vector(mode="list")
# derivative of  output (not cost/MSE!!!)
# derivative of atan(x) is 1/(1+x^2)
  if (linear_output)
  {
    dA_listh[[1]]<-rep(1,m)
  } else
  {
    if (atan_not_sigmoid)
    {
      dA_listh[[1]]<-1/(1+cache$Z_list[[length(cache$Z_list)]]^2)
    } else
    {
      dA_listh[[1]]<-(1-cache$A_list[[length(cache$A_list)]])*cache$A_list[[length(cache$A_list)]]
    }
  }
  dim(dA_listh[[1]])
  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))#i<-2
    {
# Runs backwards i.e. ordering of n_h is reversed
      if (atan_not_sigmoid)
      {
        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])/(1+cache$Z_list[[length(n_h)+2-i]]^2)
      } else
      {
        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])*(1-cache$A_list[[length(n_h)+2-i]])*cache$A_list[[length(n_h)+2-i]]
      }
    }
  } else
  {
    i<-1
  }

# Input layer
  if (atan_not_sigmoid)
  {
    dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])/(1+cache$Z_list[[1]]^2)
  } else
  {
    dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])*(1-cache$A_list[[1]])*cache$A_list[[1]]
  }
  #  dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])*(1-cache$A_list[[1]])*cache$A_list[[1]]

  dim(dA_listh[[i+1]])
  dim(params$W_list[[1]])
# LPD for each time point
  LPD_t<-t(t(params$W_list[[1]])%*% dA_listh[[i+1]])
# mean LPD over all time points
  LPD<-1/m *apply(LPD_t,2,sum)
  # Additional scaling in the case of sigmoid
  if (!atan_not_sigmoid|linear_output)
  {
#    LPD<-LPD*4^(length(neuron_vec))
#    LPD_t<-LPD_t*4^(length(neuron_vec))

  }
  dA_list<-dA_listh
#  for (i in 1:length(dA_listh))
#  {
#    dA_list[[i]]<-dA_listh[[length(dA_listh)+1-i]]
#  }

  return(list(LPD=LPD,dA_list=dA_list,LPD_t=LPD_t))
}



#params<-parm
# As above but less awkward because gradient is computed from input to output (rather than 'backpropagated')
# Derivative of output (not cost/MSE) with respect to input data (not parameters)
# Here we don't use backward propagation
# Instead the gradient dA_list_forward is computed at each layer sequentially from input through hidden to output
# This mathematically more coherent 'forward' gradient calculation is needed when computing the Hessian
LPD_forward <- function(cache, params, list_layer_size,linear_output,atan_not_sigmoid)
{
  #params<-parm

  m <- dim(cache$A_list[[length(cache$A_list)]])[2]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  dA_listh <- vector(mode="list")
  # For each explanatory variable k
  da<-array(dim=c(m,n_x,n_h[1]))
  for (k in 1:n_x)#k<-1
  {
    if (atan_not_sigmoid)
    {
      da[,k,]<-t(params$W_list[[1]][,k]*(1/(1+cache$Z_list[[1]]^2)))
    } else
    {
      da[,k,]<-t(params$W_list[[1]][,k]*cache$A_list[[1]]*(1-cache$A_list[[1]]))
    }
  }
  dA_listh[[1]]<-da
#  dA_listh[[1]][,,1]

#  dim(dA_listh[[1]])
  # Hidden to hidden
  if (length(n_h)>1)
  {
    for (i in 2:(length(n_h)))#i<-2
    {
      da<-array(dim=c(m,n_x,n_h[i]))
      for (k in 1:n_x)#k<-1
      {
        if (atan_not_sigmoid)
        {
#          dim(dA_listh[[i-1]][,k,])
#          dim(params$W_list[[i]])
#          dim((1/(1+cache$Z_list[[i]]^2)))
          da[,k,]<-t(params$W_list[[i]]%*%t(matrix(dA_listh[[i-1]][,k,],nrow=m))*(1/(1+cache$Z_list[[i]]^2)))
        } else
        {
          da[,k,]<-t(params$W_list[[i]]%*%t(matrix(dA_listh[[i-1]][,k,],nrow=m))*cache$A_list[[i]]*(1-cache$A_list[[i]]))
        }
      }
      dA_listh[[i]]<-da
    }
  } else
  {
    i<-1
  }
    # Last hidden to output layer
  da<-matrix(nrow=m,ncol=n_x)
  for (k in 1:n_x)#k<-1
  {
    if (linear_output)
    {
# Check if dA_listh is vector or matrix: transposition doesn't work the same...
      if (is.vector(dA_listh[[i]][,k,]))
      {
        da[,k]<-params$W_list[[i+1]]%*%as.matrix(dA_listh[[i]][,k,])
      } else
      {
        da[,k]<-params$W_list[[i+1]]%*%t(as.matrix(dA_listh[[i]][,k,]))
      }
    } else
    {
      if (atan_not_sigmoid)
      {
        da[,k]<-params$W_list[[i+1]]%*%t(matrix(dA_listh[[i]][,k,],nrow=m))*(1/(1+cache$Z_list[[i+1]]^2))
      } else
      {
        da[,k]<-params$W_list[[i+1]]%*%t(matrix(dA_listh[[i]][,k,],nrow=m))*cache$A_list[[i+1]]*(1-cache$A_list[[i+1]])
      }
    }

    dA_listh[[i+1]]<-da

  }
  dA_list_forward<- dA_listh
  return(list(dA_list_forward=dA_list_forward))
}
#dA_list_forward[[length(dA_list_forward)]]
#dA_list_forward[[2]]











#params<-parm
#  j<-1
#   k<-1


# Hessian of output with respect to x_j, x_k
# Tough call because it mixes backpropagation (in dA_list) with forward propagation (dA_list_forward)
#   In the derivative of the complex gradient product one needs at each layer the gradient from input to hidden (forward prop)
#   But the Hessian is contructed recursively from output to input (backpropagation)
Hessian_kj <- function(dA_list,cache, params, list_layer_size,linear_output,atan_not_sigmoid,dA_list_forward,k,j)
{
  # params<-parm
  m <- dim(cache$A_list[[length(cache$A_list)]])[2]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  Hessian_t<-NULL
  ddA_list <- vector(mode="list")
# Derivative of  output (not cost/MSE!!!)
# Here derivative with respect to x_k i.e. dA_list and dA_list_forward are derivatives with respect to x_k
  if (linear_output)
  {
    ddA_list[[1]]<-rep(0,m)
  } else
  {
    if (atan_not_sigmoid)
    {
# Derivative of atan(x) is 1/(1+x^2): double derivative is -2x/(1+x^2)^2
      ddA_list[[1]]<-(-2*cache$Z_list[[length(cache$Z_list)]]/(1+cache$Z_list[[length(cache$Z_list)]]^2)^2)*
        params$W_list[[length(cache$Z_list)]]%*%t(dA_list_forward[[length(cache$Z_list)-1]][,k,])
    } else
    {
# Derivative of phi(x) is phi(x)(1-phi(x)): double derivative is (1-phi(x))phi(x)(1-2phi(x))
      da<-(1-cache$A_list[[length(cache$A_list)]])*cache$A_list[[length(cache$A_list)]]
      ddA_list[[1]]<-da*(1-2*cache$A_list[[length(cache$A_list)]])*
      params$W_list[[length(cache$Z_list)]]%*%t(dA_list_forward[[length(cache$Z_list)-1]][,k,])
    }
  }
#    dim(ddA_list[[1]])
  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))#i<-2
    {
# Runs backwards i.e. ordering of n_h is reversed
      if (atan_not_sigmoid)
      {
          #        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])/(1+cache$Z_list[[length(n_h)+2-i]]^2)
        ddA_list[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% ddA_list[[i-1]])*1/(1+cache$Z_list[[length(n_h)+2-i]]^2)+
          (t(params$W_list[[length(n_h)+3-i]])%*% dA_list[[i-1]])*(-2)*cache$Z_list[[length(n_h)+2-i]]/(1+cache$Z_list[[length(n_h)+2-i]]^2)^2*
          params$W_list[[length(n_h)+2-i]]%*%t(dA_list_forward[[length(n_h)+1-i]][,k,])
      } else
      {
          #        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])*(1-cache$A_list[[length(n_h)+2-i]])*cache$A_list[[length(n_h)+2-i]]
        da<-(1-cache$A_list[[length(n_h)+2-i]])*cache$A_list[[length(n_h)+2-i]]
        ddA_list[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% ddA_list[[i-1]])*da+
          (t(params$W_list[[length(n_h)+3-i]])%*% dA_list[[i-1]])*da*(1-2*cache$A_list[[length(n_h)+2-i]])*
        params$W_list[[length(n_h)+2-i]]%*%t(dA_list_forward[[length(n_h)+1-i]][,k,])
      }
    }
  } else
  {
    i<-1
  }

# Input layer: still derivative with respect to x_k
  if (atan_not_sigmoid)
  {
      #    ddA_list[[i+1]]<-(t(params$W_list[[2]])%*% ddA_list[[i]])*dA_list_forward[[i]][,k,]*1/(1+cache$Z_list[[1]]^2)+
      #      (t(params$W_list[[2]])%*% dA_list[[i]])*(-2)*cache$Z_list[[1]]/(1+cache$Z_list[[1]]^2)^2
    ddA_list[[i+1]]<-(t(params$W_list[[2]])%*% ddA_list[[i]])*1/(1+cache$Z_list[[1]]^2)+
      (t(params$W_list[[2]])%*% dA_list[[i]])*(-2)*cache$Z_list[[1]]/(1+cache$Z_list[[1]]^2)^2*(params$W_list[[1]][,k])
  } else
  {
    da<-(1-cache$A_list[[1]])*cache$A_list[[1]]
    ddA_list[[i+1]]<-(t(params$W_list[[2]])%*% ddA_list[[i]])*da+
      (t(params$W_list[[2]])%*% dA_list[[i]])*da*((1-2*cache$A_list[[1]]))*(params$W_list[[1]][,k])
  }

# Hessian for each time point
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Here derivative with respect to x_j
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  Hessian_t<-t(t(params$W_list[[1]][,j])%*% ddA_list[[i+1]])
# Mean Hessian over all time points
  Hessian<-1/m *apply(Hessian_t,2,sum)


  return(list(Hessian_t=Hessian_t,Hessian=Hessian,ddA_list=ddA_list))
}







# Hessian: same as above but computes entire diagonal of Hessian matrix
Hessian_diag <- function(dA_list,cache, params, list_layer_size,linear_output,atan_not_sigmoid,dA_list_forward)
{
  # params<-parm
  m <- dim(cache$A_list[[length(cache$A_list)]])[2]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  Hessian_t<-NULL
  for (k in 1:n_x)
  {
    ddA_list <- vector(mode="list")
    # Derivative of  output (not cost/MSE!!!)
    if (linear_output)
    {
      ddA_list[[1]]<-rep(0,m)
    } else
    {
      if (atan_not_sigmoid)
      {
        # Derivative of atan(x) is 1/(1+x^2)
        ddA_list[[1]]<-(-2*cache$Z_list[[length(cache$Z_list)]]/(1+cache$Z_list[[length(cache$Z_list)]]^2)^2)*
          params$W_list[[length(cache$Z_list)]]%*%t(dA_list_forward[[length(cache$Z_list)-1]][,k,])
      } else
      {
        da<-(1-cache$A_list[[length(cache$A_list)]])*cache$A_list[[length(cache$A_list)]]
        ddA_list[[1]]<-da*(1-2*cache$A_list[[length(cache$A_list)]])*
          params$W_list[[length(cache$Z_list)]]%*%t(dA_list_forward[[length(cache$Z_list)-1]][,k,])
      }
    }
    #    dim(ddA_list[[1]])
    if (length(n_h)>1)
    {
      for (i in 2:length(n_h))#i<-2
      {
        # Runs backwards i.e. ordering of n_h is reversed
        if (atan_not_sigmoid)
        {
          #        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])/(1+cache$Z_list[[length(n_h)+2-i]]^2)
          ddA_list[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% ddA_list[[i-1]])*1/(1+cache$Z_list[[length(n_h)+2-i]]^2)+
            (t(params$W_list[[length(n_h)+3-i]])%*% dA_list[[i-1]])*(-2)*cache$Z_list[[length(n_h)+2-i]]/(1+cache$Z_list[[length(n_h)+2-i]]^2)^2*
            params$W_list[[length(n_h)+2-i]]%*%t(dA_list_forward[[length(n_h)+1-i]][,k,])
        } else
        {
          #        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])*(1-cache$A_list[[length(n_h)+2-i]])*cache$A_list[[length(n_h)+2-i]]
          da<-(1-cache$A_list[[length(n_h)+2-i]])*cache$A_list[[length(n_h)+2-i]]
          ddA_list[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% ddA_list[[i-1]])*da+
            (t(params$W_list[[length(n_h)+3-i]])%*% dA_list[[i-1]])*da*(1-2*cache$A_list[[length(n_h)+2-i]])*
            params$W_list[[length(n_h)+2-i]]%*%t(dA_list_forward[[length(n_h)+1-i]][,k,])
        }
      }
    } else
    {
      i<-1
    }

    # Input layer
    if (atan_not_sigmoid)
    {
      #    ddA_list[[i+1]]<-(t(params$W_list[[2]])%*% ddA_list[[i]])*dA_list_forward[[i]][,k,]*1/(1+cache$Z_list[[1]]^2)+
      #      (t(params$W_list[[2]])%*% dA_list[[i]])*(-2)*cache$Z_list[[1]]/(1+cache$Z_list[[1]]^2)^2
      ddA_list[[i+1]]<-(t(params$W_list[[2]])%*% ddA_list[[i]])*1/(1+cache$Z_list[[1]]^2)+
        (t(params$W_list[[2]])%*% dA_list[[i]])*(-2)*cache$Z_list[[1]]/(1+cache$Z_list[[1]]^2)^2*(params$W_list[[1]][,k])
    } else
    {
      da<-(1-cache$A_list[[1]])*cache$A_list[[1]]
      ddA_list[[i+1]]<-(t(params$W_list[[2]])%*% ddA_list[[i]])*da+
        (t(params$W_list[[2]])%*% dA_list[[i]])*da*((1-2*cache$A_list[[1]]))*(params$W_list[[1]][,k])
    }
    #  dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])*(1-cache$A_list[[1]])*cache$A_list[[1]]

    dim(ddA_list[[i+1]])
    dim(params$W_list[[1]])
    # LPD for each time point
    Hessian_t<-cbind(Hessian_t,t(t(params$W_list[[1]][,k])%*% ddA_list[[i+1]]))
  }
  # mean LPD over all time points
  #  Hessian<-1/m *apply(t(params$W_list[[1]])%*% ddA_list[[i+1]],1,sum)
  # Additional scaling in the case of sigmoid
  if (!atan_not_sigmoid|linear_output)
  {
    #    LPD<-LPD*4^(length(neuron_vec))
    #    LPD_t<-LPD_t*4^(length(neuron_vec))

  }
  #  for (i in 1:length(dA_listh))
  #  {
  #    dA_list[[i]]<-dA_listh[[length(dA_listh)+1-i]]
  #  }
  Hessian<-1/m *apply(Hessian_t,2,sum)

  return(list(Hessian_t=Hessian_t,Hessian=Hessian,ddA_list=ddA_list))
}






# This was used for checking Hessian and correcting bugs based on toy neural net
dd_func<-function()
{

  # This is true Hessian for toy example
  -2*cache$Z_list[[length(cache$Z_list)]]/(1+cache$Z_list[[length(cache$Z_list)]]^2)^2*
    params$W_list[[2]][1]*(1/(1+cache$Z_list[[1]]^2))*
    params$W_list[[1]][1]*(1/(1+cache$Z_list[[1]]^2))*params$W_list[[1]][1]*params$W_list[[2]][1]+
    params$W_list[[1]][1]*params$W_list[[2]][1]*(1/(1+cache$Z_list[[length(cache$Z_list)]]^2))*
    params$W_list[[1]][1]*(-2*cache$Z_list[[1]]/(1+cache$Z_list[[1]]^2)^2)

  # OK: This one (first line) matches DDA_list[[1]]
  -2*cache$Z_list[[length(cache$Z_list)]]/(1+cache$Z_list[[length(cache$Z_list)]]^2)^2
  ddA_list[[1]]

  # Next: this one matches first two lines
  (t(params$W_list[[2]])%*% ddA_list[[i]])/(1+cache$Z_list[[1]]^2)

  -2*cache$Z_list[[length(cache$Z_list)]]/(1+cache$Z_list[[length(cache$Z_list)]]^2)^2*
    params$W_list[[2]][1]*(1/(1+cache$Z_list[[1]]^2))

  # Problem: part of third line is missing!!! Should square (1/(1+cache$Z_list[[1]]^2))*params$W_list[[1]][1]*params$W_list[[2]][1]

  -2*cache$Z_list[[length(cache$Z_list)]]/(1+cache$Z_list[[length(cache$Z_list)]]^2)^2*
    params$W_list[[2]][1]*(1/(1+cache$Z_list[[1]]^2))*
    params$W_list[[1]][1]#*(1/(1+cache$Z_list[[1]]^2))*params$W_list[[1]][1]*params$W_list[[2]][1]


  t(params$W_list[[1]][1])*((t(params$W_list[[2]])%*% ddA_list[[i]])/(1+cache$Z_list[[1]]^2))

  # 4.th and 5th lines

  params$W_list[[1]][1]*params$W_list[[2]][1]*(1/(1+cache$Z_list[[length(cache$Z_list)]]^2))*
    params$W_list[[1]][1]*(-2*cache$Z_list[[1]]/(1+cache$Z_list[[1]]^2)^2)


  params$W_list[[1]][1]*(1/(1+cache$Z_list[[1]]^2))*params$W_list[[1]][1]*params$W_list[[2]][1]


  # i=1: OK
  dd1<-params$W_list[[1]][1]*(-2*cache$Z_list[[1]]/(1+cache$Z_list[[1]]^2)^2)



  # i=2:
  params$W_list[[2]][1]*(1/(1+cache$Z_list[[length(cache$Z_list)]]^2))*params$W_list[[1]][1]*dd1+
    params$W_list[[2]][1]*(-2)*cache$Z_list[[length(cache$Z_list)]]/(1+cache$Z_list[[length(cache$Z_list)]]^2)^2*
    dA_list_forward[[1]][,k,]*dA_list_forward[[1]][,k,]*params$W_list[[2]][1]



  params$W_list[[i+1]]%*%t(matrix(ddA_listh[[i]][,k,],nrow=m))*params$W_list[[i]][,k]%*%(1/(1+cache$Z_list[[i+1]]^2))
  params$W_list[[2]][1]*dd1*params$W_list[[1]][1]*(1/(1+cache$Z_list[[length(cache$Z_list)]]^2))
}

















# Gradient of MSE with respect to input data x (backprop is gradient of MSE with respect to weights/biases)
LPD_cost <- function(y_train, cache, params, list_layer_size,linear_output,atan_not_sigmoid)
{


  m <- dim(y_train)[1]

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  dA_list<-dA_listh <- vector(mode="list")

  # derivative of MSE at output: derivative of atan(x) is 1/(1+x^2)
  if (linear_output)
  {
    dA_listh[[1]]<-2*(cache$A_list[[length(cache$A_list)]]-as.vector(y_train))
  } else
  {
    if (atan_not_sigmoid)
    {
      dA_listh[[1]]<-2*(cache$A_list[[length(cache$A_list)]]-as.vector(y_train))*(1-cache$A_list[[length(cache$A_list)]]^2)
      dA_listh[[1]]<-2*(cache$A_list[[length(cache$A_list)]]-as.vector(y_train))/(1+cache$Z_list[[length(cache$Z_list)]]^2)
    } else
    {
      dA_listh[[1]]<-2*(cache$A_list[[length(cache$A_list)]]-as.vector(y_train))*(1-cache$A_list[[length(cache$A_list)]])*cache$A_list[[length(cache$A_list)]]
    }
  }

  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))#i<-2
    {
      # Runs backwards i.e. ordering of n_h is reversed
      if (atan_not_sigmoid)
      {
        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])*(1-cache$A_list[[length(n_h)+2-i]]^2)
        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])/(1+cache$Z_list[[length(n_h)+2-i]]^2)
      } else
      {
        dA_listh[[i]]<-(t(params$W_list[[length(n_h)+3-i]])%*% dA_listh[[i-1]])*(1-cache$A_list[[length(n_h)+2-i]])*cache$A_list[[length(n_h)+2-i]]
      }
    }
  } else
  {
    i<-1
  }

  # Input layer
  if (atan_not_sigmoid)
  {
    dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])*(1-cache$A_list[[1]]^2)
    dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])/(1+cache$Z_list[[1]]^2)
  } else
  {
    dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])*(1-cache$A_list[[1]])*cache$A_list[[1]]
  }
  #  dA_listh[[i+1]]<-(t(params$W_list[[2]])%*% dA_listh[[i]])*(1-cache$A_list[[1]])*cache$A_list[[1]]

  dim(dA_listh[[i+1]])
  dim(params$W_list[[1]])
  LPD_cost_t<-t(params$W_list[[1]])%*% dA_listh[[i+1]]
  LPD_cost<-apply(LPD_cost_t,1,mean)
  # Additional scaling in the case of sigmoid
#  if (!atan_not_sigmoid|linear_output)
#    LPD_cost<-LPD_cost*4^(length(neuron_vec))
  dA_list<-dA_listh

  return(list(LPD_cost=LPD_cost,dA_list=dA_list,LPD_cost_t=LPD_cost_t))
}










# Train the model: no regularization
#y<-y_train
trainModel <- function(x_train, y, hyper_list)
{

  epochs<-hyper_list$epochs
  learning_rate<-hyper_list$learning_rate
  linear_output<-hyper_list$linear_output
  atan_not_sigmoid<-hyper_list$atan_not_sigmoid
  neuron_vec<-hyper_list$neuron_vec

  layer_size <- getLayerSize(x_train, y, neuron_vec)

  if (is.null(hyper_list$parm_init))
  {
    init_params <- initializeParameters( layer_size)
  } else
  {
    init_params <- hyper_list$parm_init
  }
    #  if (F)
#  {
#    nn <- neuralnet(y~. ,data=x_train,hidden=hyper_list$neuron_vec,linear.output=F)

#    for (i in 1:length(init_params$W_list))#i<-1
#    {
#      init_params$b_list[[i]]<-nn$weights[[1]][[i]][1,]
#      init_params$W_list[[i]]<-t(nn$weights[[1]][[i]][2:nrow(nn$weights[[1]][[i]]),])
#    }
#  }
  cost_history <- c()
  pb <- txtProgressBar(min = 1, max = epochs, style = 3)
#i<-1
  cost_min<-10^90
  for (i in 1:epochs) {
    fwd_prop <- forwardPropagation(x_train, init_params, layer_size,linear_output,atan_not_sigmoid)
    cost <- computeCost(y, fwd_prop)
    back_prop <- backwardPropagation(x_train, y, fwd_prop, init_params, layer_size,linear_output,atan_not_sigmoid)
    update_params <- updateParameters(back_prop, init_params, learning_rate = learning_rate)
    init_params <- update_params
    cost_history <- c(cost_history, min(cost,cost_min))
    setTxtProgressBar(pb, i)
# We here store the overall minimum
#   Sometimes the gradient-step leads to a worse performance
#   We here store the overall best solution
    if (cost<cost_min)
    {
      par_min<-update_params
      cost_min<-cost
    }

    if (i %% 100 == 0) cat("Iteration", i, " | Cost: ", cost_min, "\n")
  }
# Complete history with last optimization step  
  fwd_prop <- forwardPropagation(x_train, init_params, layer_size,linear_output,atan_not_sigmoid)
  cost <- computeCost(y, fwd_prop)
  cost_history <- c(cost_history, min(cost,cost_min))
  
  close(pb)

  model_out <- list("updated_params" = par_min,
                    "cost_hist" = cost_history)
  return (model_out)
}


#params<-init_params
updateParameters <- function(grads, params, learning_rate){

  updated_params<-params
  for (i in 1:length(params$W_list))
  {
    # Direction: negative gradient
    updated_params$W_list[[i]]<- updated_params$W_list[[i]]-learning_rate*(grads$dW_list[[i]])
    updated_params$b_list[[i]]<- updated_params$b_list[[i]]-learning_rate*(grads$db_list[[i]])
  }

  return (updated_params)
}



# Same as above but with additional regularization weight lambda
#   -lambda is either a scalar (constant for all layers) or a vector of layer-specific regularization-weights
#   -lambda=0 means: ordinary MSE (no regularization)
# The main difference appears in the updating-part i.e. the function updateParameters_L2reg
trainModel_L2reg <- function(x_train, y, hyper_list,lambda=0)
{
# y<-y_train
# lambda<-lambda_vec
  epochs<-hyper_list$epochs
  learning_rate<-hyper_list$learning_rate
  linear_output<-hyper_list$linear_output
  atan_not_sigmoid<-hyper_list$atan_not_sigmoid
  neuron_vec<-hyper_list$neuron_vec
  if (!is.null(hyper_list$layer_size))
    layer_size<-hyper_list$layer_size

# If lambda is not a vector or if length does not match number of layers
#   then we assume lambda=lambda[1] for each layer
  if (length(lambda)==1|length(lambda)!=length(layer_size$n_h)+1)
    lambda<-rep(lambda[1],length(layer_size$n_h)+1)

  layer_size <- getLayerSize(x_train, y, neuron_vec)

  if (is.null(hyper_list$parm_init))
  {
    init_params <- initializeParameters( layer_size)
  } else
  {
    init_params <- hyper_list$parm_init
  }
  #  if (F)
  #  {
  #    nn <- neuralnet(y~. ,data=x_train,hidden=hyper_list$neuron_vec,linear.output=F)

  #    for (i in 1:length(init_params$W_list))#i<-1
  #    {
  #      init_params$b_list[[i]]<-nn$weights[[1]][[i]][1,]
  #      init_params$W_list[[i]]<-t(nn$weights[[1]][[i]][2:nrow(nn$weights[[1]][[i]]),])
  #    }
  #  }
  cost_history <- c()
  pb <- txtProgressBar(min = 1, max = epochs, style = 3)
  #i<-1
  cost_min<-10^90
  for (i in 1:epochs)#i<-1
  {
    fwd_prop <- forwardPropagation(x_train, init_params, layer_size,linear_output,atan_not_sigmoid)
    cost <- computeCost(y, fwd_prop)
    back_prop <- backwardPropagation(x_train, y, fwd_prop, init_params, layer_size,linear_output,atan_not_sigmoid)
# Here is the difference when applying regularization
    update_params <- updateParameters_L2reg(back_prop, init_params, learning_rate = learning_rate,lambda)
    init_params <- update_params
    cost_history <- c(cost_history, min(cost,cost_min))
    setTxtProgressBar(pb, i)
    # We here store the overall minimum
    #   Sometimes the gradient-step leads to a worse performance
    #   We here store the overall best solution
    if (cost<cost_min)
    {
      par_min<-update_params
      cost_min<-cost
    }

    if (i %% 100 == 0) cat("Iteration", i, " | Cost: ", cost_min, "\n")
  }
  # Complete history with last optimization step  
  fwd_prop <- forwardPropagation(x_train, init_params, layer_size,linear_output,atan_not_sigmoid)
  cost <- computeCost(y, fwd_prop)
  cost_history <- c(cost_history, min(cost,cost_min))
  
  close(pb)

  model_out <- list("updated_params" = par_min,
                    "cost_hist" = cost_history)
  return (model_out)
}


#params<-init_params
#grads<-back_prop
# Regularization:
#   Scale/normalize regularization weight lambba by number of parameters
updateParameters_L2reg <- function(grads, params, learning_rate,lambda)
{

  updated_params<-params
# Determine total number of weights
  num_par<-0
  for (i in 1:length(updated_params$W_list))
    num_par<-num_par+prod(dim(params$W_list[[i]]))
# We here apply regularization to weights w only (not biases)

  for (i in 1:length(params$W_list))#i<-1
  {
# Regularization term: divide by total number of parameters in order to robustify against high-dimensionality
    lambda_normalized<-lambda[i]/num_par
# Direction: negative gradient: up to here this is the same as updateParameters above (without regularization)
    updated_params$W_list[[i]]<- updated_params$W_list[[i]]-learning_rate*grads$dW_list[[i]]
# This is new: add derivative of squared sum of parameters
    updated_params$W_list[[i]]<-updated_params$W_list[[i]]-learning_rate*2*lambda_normalized*params$W_list[[i]]
# We here do not apply regularization to biases
    updated_params$b_list[[i]]<- updated_params$b_list[[i]]-learning_rate*(grads$db_list[[i]])
  }

  return (updated_params)
}



#----------------------------------------------------------------------------------------------


compute_number_parameters<-function(layer_size)
{
  n_h<-layer_size$n_h
  n_y<-layer_size$n_y
  n_x<-layer_size$n_x

  index<-(n_h[1] * n_x)
  index<-index+n_h[1]
  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))
    {
      index<-index+n_h[i] * n_h[i-1]
      index<-index+n_h[i]
    }
  }
  index<-index+(n_y * n_h[length(n_h)])
  index<-index+n_y
  return(index)
}



translate_Parameters <- function(list_layer_size,parm)
{

  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y

  W_list <- vector(mode="list")
  b_list <- vector(mode="list")
  W_list[[1]]<-matrix(parm[1:(n_h[1] * n_x)], nrow = n_h[1], ncol = n_x, byrow = TRUE)
  index<-(n_h[1] * n_x)
  b_list[[1]]<-matrix(parm[index+(1:n_h[1])], nrow = n_h[1])
  index<-index+n_h[1]

  i<-1
  if (length(n_h)>1)
  {
    for (i in 2:length(n_h))#i<-2
    {
      W_list[[i]]<-matrix(parm[index+(1:(n_h[i] * n_h[i-1]))], nrow = n_h[i], ncol = n_h[i-1], byrow = TRUE)
      index<-index+n_h[i] * n_h[i-1]
      b_list[[i]]<-matrix(parm[index+(1:n_h[i])], nrow = n_h[i])
      index<-index+n_h[i]
    }
  }
  W_list[[i+1]]<-matrix(parm[index+(1:(n_y * n_h[length(n_h)]))], nrow = n_y, ncol = n_h[length(n_h)], byrow = TRUE)
  index<-index+(n_y * n_h[length(n_h)])
  b_list[[i+1]]<- matrix(parm[index+(1:n_y)], nrow = n_y)
  index<-index+n_y
  param_list <- list("W_list" = W_list,"b_list"=b_list)

  return (param_list)
}








optimize_nlminb_net<-function(parm_init,x,y,layer_size,linear_output,atan_not_sigmoid)
{

  param_list<-translate_Parameters(layer_size,parm_init)

  fwd_prop <- forwardPropagation(x, param_list, layer_size,linear_output,atan_not_sigmoid)

  cache<-fwd_prop

  # https://rviews.rstudio.com/2020/07/24/building-a-neural-net-from-scratch-using-r-part-2/

  cost <- computeCost(y, fwd_prop)
  cost


  return(cost)
}


# This function estimates net-parameters with/without regularization and computes out-of-sample MSE and LPD
compute_net_func<-function(x_train, y_train,x_test,y_test, hyper_list,lambda,setseed,layer_size)
{
  linear_output<-hyper_list$linear_output
  atan_not_sigmoid<-hyper_list$atan_not_sigmoid
  
  set.seed(setseed)
  
  train_model <- trainModel_L2reg(x_train, y_train, hyper_list=hyper_list,lambda)
  
  updated_params<-train_model$updated_params
  # Out-of-sample output of optimized net
  fwd_prop <- forwardPropagation(x_test, updated_params, layer_size,linear_output,atan_not_sigmoid)
  cache<-fwd_prop
  output<-fwd_prop$A_list[[length(fwd_prop$A_list)]]
  # Output
  as.double(output)
  # MSE of optimized net
  cost <- computeCost(y_test, fwd_prop)
  cost
  
  LPD_obj<-LPD( cache, updated_params, list_layer_size,linear_output,atan_not_sigmoid)
  
  LPD_t<-LPD_obj$LPD_t
  
  return(list(cost=cost,train_model=train_model,LPD_t=LPD_t))
}



