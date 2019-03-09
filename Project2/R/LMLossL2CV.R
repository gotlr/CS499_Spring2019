#' Cross validation using linear model with L2 regularization and squareloss
#' @param x.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param fold.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#' @return result.list a list with mean.validation.loss.vec,
#' mean.train.loss.vec,penalty.vec,selected.penalty,weight.vec,and predict function

LMSquareLossL2CV <- function(X.mat, y.vec, fold.vec, penalty.vec){
  if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec),is.numeric(y.vec),length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(is.integer(fold.vec), is.vector(fold.vec))) {
    stop("fold.vec must be assigned before input and it must be a integer vector")
  }
  penalty.descending <- function(penalty.vec){
    if.decending <- all(diff(penalty.vec)<0)
    return(if.decending)
    
  }
  
  if (!all(is.vector(penalty.vec),is.numeric(penalty.vec),penalty.vec >= 0,is.decending(penalty.vec))) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  #fold.number=4
  #fold.split=sample(1:fold.number,length(fold.vec),replace=T)
  max.fold <- length(fold.vec)
  validation.loss.mat <-matrix(0, fold.number, max.iteration)
  train.loss.mat <- matrix(0, n.folds, max.iteration)#initial the condition
  for (i in max.fold){
    train.index <- which(fold.split !=i)
    validation.index <- which(fold.split == i)
    w.mat <-LMSquareLossL2penalties(x.mat[train.index, ], y.vec[train.index, ], penalty.vec)
    train.loss <- ((x.mat[train.index, ] %*% w.mat)-y.vec[train.index, ])^2
    validation.loss <- ((x.mat[validation.index, ] %*% w.mat)-y.vec[train.index, ])^2
    mean.train.loss.vec <- colMeans(train.loss)
    mean.validation.loss.vec <- colMeans(validation.loss)
    train.loss.mat[fold.number, ] = mean.train.loss.vec
    validation.loss.mat[fold.number, ] = mean.validation.loss.vec
    
  }
  
  mean.train.loss.vec <- colMeans(train.loss.mat)
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  selected.penalty <-penalty.vec[which.min(mean.validation.loss.vec)]
  W.mat <-LMSquareLossL2penalties(X.mat[train.index, ], y.vec[train.index, ], penalty.vec)
  weight.vec <- W.mat[,selected.penalty]
  predict <- function(testX.mat) {
    prediction.vec <- testX.mat %*% weight.vec
  }
  
  Output <- list(
    mean.validation.loss.vec = mean.validation.loss.vec,
    mean.train.loss.vec = mean.train.loss.vec,
    penalty.vec = penalty.vec,
    selected.penalty = selected.penalty,
    weight.vec = weight.vec,
    predict
  )
  
  return(Output)
  
}

#' Cross validation using linear model with L2 regularization and logisticloss
#' @param x.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric vector of length nrow(X.mat)
#' @param fold.vec a numeric vector of length nrow(X.mat)
#' @param penalty.vec a non-negative numeric vector
#' @return result.list a list with mean.validation.loss.vec,
#' mean.train.loss.vec,penalty.vec,selected.penalty,weight.vec,and predict function




LMLogisticLossL2CV  <- function(X.mat, y.vec, fold.vec, penalty.vec){
  if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
    stop("X.mat must be a numeric matrix.")
  }
  
  if (!all(is.vector(y.vec),is.numeric(y.vec),length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
  }
  
  if (!all(is.integer(fold.vec), is.vector(fold.vec))) {
    stop("fold.vec must be assigned before input and it must be a integer vector")
  }
  
  if (!all(is.vector(penalty.vec),is.numeric(penalty.vec),penalty.vec >= 0 )) {
    stop("penalty.vec must be a non-negative decreasing numeric vector")
  }
  
  
  fold.vec=unique(fold.vec)
  max.fold=length(fold.vec)
  fold.split=sample(1:max.fold,max.fold,replace=F)
  #max.fold <- length(fold.vec)
  validation.loss.mat <-matrix(0, max.fold, max.iterations)
  train.loss.mat <- matrix(0, max.fold, max.iterations)#initial the condition
  
  for (i in max.fold){
    train.index <- which(fold.split !=i)
    validation.index <- which(fold.split == i)
    w.mat <-LMLogisticLossL2penalties(x.mat[train.index, ], y.vec[train.index, ], penalty.vec)
    train.loss <- ((x.mat[train.index, ] %*% w.mat)-y.vec[train.index])^2
    validation.loss <- ((cbind(1,x.mat[train.index, ]) %*% w.mat)-y.vec[train.index])^2
    mean.train.loss.vec <- colMeans(train.loss)
    mean.validation.loss.vec <- colMeans(validation.loss)
    train.loss.mat[i, ] = mean.train.loss.vec
    validation.loss.mat[i, ] = mean.validation.loss.vec
    
  }
  
  mean.train.loss.vec <- colMeans(train.loss.mat)
  mean.validation.loss.vec <- colMeans(validation.loss.mat)
  selected.penalty <-penalty.vec[which.min(mean.validation.loss.vec)]
  W.mat <-LMLogisticLossL2penalties(X.mat[train.index, ], y.vec[train.index], penalty.vec)
  weight.vec <- W.mat[,selected.penalty]
  
  predict <- function(testX.mat) {
    prediction.vec <- testX.mat %*% weight.vec
  }
  
  Output <- list(
    mean.validation.loss.vec = mean.validation.loss.vec,
    mean.train.loss.vec = mean.train.loss.vec,
    penalty.vec = penalty.vec,
    selected.penalty = selected.penalty,
    weight.vec = weight.vec,
    predict
  )
  
  return(Output)
  
}