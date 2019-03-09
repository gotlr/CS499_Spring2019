#' Linear model with L2 penalties and squareloss
#' @param x.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#' @return W.mat a numeric weight matrix of size [ncol(X.mat) x length(penalty.vec)]




LMSquareLossL2penalties <- function(X.mat, y.vec, penalty.vec){
  if (!all(is.matrix(X.scaled.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec),
           is.numeric(y.vec),
           length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(length(penalty) == 1, is.numeric(penalty), penalty >= 0)) {
    stop("penalty must be a non-negative numeric scalar.")
  }
  
  if (!all(is.vector(penalty.vec),is.numeric(penalty.vec),penalty.vec >= 0)) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  
  train.number <-  dim(x.mat)[1]
  feature.number <-  dim(x.mat)[2]
  x.mean <- colMeans(x.mat) #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column
  x.sd <- sqrt(rowSums((t(x.mat) - x.mean) ^ 2)/train.number)
  x.sd.mat <- diag(x.sd)
  x.scaled <- t((t(x.mat) -x.mean) / x.sd)
  w.mat.temp=matrix(rep(0,feature.number))
  w.vec=matrix(rep(0,feature.number))
  for(iteration in (1:NCOL(penalty.vec))){# calling LM__LossL2 to get the (scaled) optimal weight vector for each.
    optimal.weight.vec <- LMSquareLossL2(x.scaled,y.vec,penalty=penalty.vec[index],initial.weight.vec=w.vec)#the initial condition of w is 0 for the first penalty?
    w.mat.temp=cbind(w.mat.temp,optimal.weight.vec)
    w.vec <- optimal.weight.vec#use the optimal solution for the previous penalty value as the next initial.weight.vec (faster).
  }
  w.mat.temp=w.mat.temp[,-1]#omit the first initial value(0)
  intercept <- -t(w.mat.temp)%*%x.sd.mat%*%x.mean #m*p*p*p*P*1=m*1
  w.mat <- rbind(t(intercept),t(t(w.mat.temp)%*%x.sd.mat))
  return(w.mat)#(p+1) x m
  
}


#' Linear model with L2 penalties and logitsicloss
#' @param x.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#' @return W.mat a numeric weight matrix of size [ncol(X.mat) x length(penalty.vec)]

LMLogisticLossL2penalties <- function(X.mat, y.vec, penalty.vec){
  
  if (!all(is.matrix(X.scaled.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec),
           is.numeric(y.vec),
           length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(length(penalty) == 1, is.numeric(penalty), penalty >= 0)) {
    stop("penalty must be a non-negative numeric scalar.")
  }
  
  if (!all(is.vector(penalty.vec),is.numeric(penalty.vec),penalty.vec >= 0)) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  train.number <-  dim(x.mat)[1]
  feature.number <-  dim(x.mat)[2]
  x.mean <- colMeans(x.mat) #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column
  x.sd <- sqrt(rowSums((t(x.mat) - x.mean) ^ 2)/train.number)
  x.sd.mat <- diag(x.sd)
  x.scaled <- t((t(x.mat) -x.mean) / x.sd)
  w.mat.temp=matrix(rep(0,feature.number))
  w.vec=matrix(rep(0,feature.number))
  for(iteration in (1:NCOL(penalty.vec))){# calling LM__LossL2 to get the (scaled) optimal weight vector for each.
    optimal.weight.vec <- LMLogisticLossL2(x.scaled,y.vec,penalty=penalty.vec[index],initial.weight.vec=w.vec)#the initial condition of w is 0 for the first penalty?
    w.mat.temp=cbind(w.mat.temp,optimal.weight.vec)
    w.vec <- optimal.weight.vec#use the optimal solution for the previous penalty value as the next initial.weight.vec (faster).
  }
  w.mat.temp=w.mat.temp[,-1]#omit the first initial value(0)
  intercept <- -t(w.mat.temp)%*%x.sd.mat%*%x.mean #m*p*p*p*P*1=m*1
  w.mat <- rbind(t(intercept),t(t(w.mat.temp)%*%x.sd.mat))
  return(w.mat)#(p+1) x m
  
}