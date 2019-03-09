#' Linear model iteration with square loss
#'
#' @param x.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param max.iterations integer scalar greater than 1
#' @param step.size integer scalar
#' @return w.mat matrix of weight vectors of size [(p + 1) x max.iterations]. 



LMSquareLossIterations <-function(x.mat, y.vec, max.iterations, step.size) {
  if (!all(is.matrix(x.mat), is.numeric(x.mat))) {
    stop("x.mat must be a numeric matrix.")
  }
  if (!all(is.integer(max.iterations),
      max.iterations > 1,
      length(max.iterations) == 1)) {
    stop("max.iterations must be a greater than 1 integer scalar number.")
  }
  
  if (!all(is.numeric(step.size), length(step.size) == 1)) {
    stop("step.size must be a numeric scalar value.")
  }
  train.number <-  dim(x.mat)[1]
  feature.number <-  dim(x.mat)[2]
  x.mean <- colMeans(x.mat) #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column
  x.sd <- sqrt(rowSums((t(x.mat) - x.mean) ^ 2)/train.number)
  x.sd.mat <- diag(x.sd)
  x.scaled <- t((t(x.mat) -x.mean) / x.sd)
  w.mat.temp=matrix(rep(0,feature.number))
  for(iteration in (1:max.iterations)){
    mean.loss=(2 * t(x.scaled) %*%(x.scaled %*% w.mat.temp[, dim(w.mat.temp)[2]] - y.vec)) / train.number
    w.vec=w.mat.temp[, dim(w.mat.temp)[2]]-step.size*mean.loss
    w.mat.temp=cbind(w.mat.temp,w.vec)
  }
  w.mat.temp=w.mat.temp[,-1]#omit the first initial value(0)
  intercept <- -t(w.mat.temp)%*%x.sd.mat%*%x.mean #m*p*p*p*P*1=m*1
  w.mat <- rbind(t(intercept),t(t(w.mat.temp)%*%x.sd.mat))
  return(w.mat)#(p+1) x m
    
}


#' Linear model iteration with logistic loss
#'
#' @param x.mat train feature matrix of size [n x p]
#' @param y.vec train label vector of size [n x 1]
#' @param max.iterations integer scalar greater than 1
#' @param step.size integer scalar
#' @return w.mat matrix of weight vectors of size [(p + 1) x max.iterations]. 

LMLogisticLossIterations <- function(X.mat, y.vec, max.iterations, step.size){
  if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  if (!all(is.integer(max.iterations),
           max.iterations > 1,
           length(max.iterations) == 1)) {
    stop("Input max.iterations must be a greater than 1 integer scalar number.")
  }
  
  if (!all(is.numeric(step.size), length(step.size) == 1)) {
    stop("step.size must be a numeric scalar value.")
  }
  train.number <-  dim(x.mat)[1]
  feature.number <-  dim(x.mat)[2]
  x.mean <- colMeans(x.mat)
  x.sd <- sqrt(rowSums((t(x.mat) - x.mean) ^ 2)/train.number)
  x.sd.mat <- diag(x.sd)
  x.scaled <- t((t(x.mat) -x.mean) / x.sd)
  w.mat.temp=matrix(rep(0,feature.number))
  sigmoid <- function(z) { 1 / (1 + exp(-z))}
  for(iteration in (1:max.iterations)){
    mean.loss <- t(x.scaled) %*% (sigmoid(x.scaled %*% w.mat.temp[, dim(w.mat.temp)[2]] )-y.vec)
    w.vec <- w.mat.temp[, dim(w.mat.temp)[2]] - mean.loss * step.size
    w.mat.temp=cbind(w.mat.temp,w.vec)
  }
  w.mat.temp=w.mat.temp[,-1]#omit the first initial value(0)
  intercept <- -t(w.mat.temp)%*%x.sd.mat%*%x.mean #m*p*p*p*P*1=m*1
  w.mat <- rbind(t(intercept),t(t(w.mat.temp)%*%x.sd.mat))
  return(w.mat)#(p+1) x m
  
  
  
}