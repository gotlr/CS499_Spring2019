LMSquareLossIterations <-function(x.mat, y.vec, max.iterations, step.size = 0.5) {
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
  x.mean <- colMeans(x.mat) #make sure to compute a scaled input matrix, which has mean=0 and sd=1 for each column
  x.sd <- sqrt(rowSums((t(x.mat) - x.mean) ^ 2)/train.number)
  x.scaled <- t((t(x.mat) -x.mean) / x.sd)
  w.mat=matrix(rep(0,feature.number))
  for(iteration in (1:max.iterations)){
    mean.loss=(2 * t(x.scaled) %*%(x.scaled %*% w.mat[, dim(w.mat)[2]] - y.vec)) / num.train
    w.vec=w.mat[, dim(w.mat)[2]]-step.size*mean.loss
    w.mat=cbind(w.mat,w.vec)
  }
    return(w.mat)#p x m
    
}


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
  x.scaled <- t((t(x.mat) -x.mean) / x.sd)
  w.mat=matrix(rep(0,feature.number))
  sigmoid <- function(z) { 1 / (1 + exp(-z))}
  for(iteration in (1:max.iterations)){
    mean.loss <- t(x.scaled) %*% (sigmoid(x.scaled %*% w.mat[, dim(w.mat)[2]] )-y.vec)
    w.vec <- w.mat[, dim(w.mat)[2]] - mean.loss * step.size
    w.mat=cbind(w.mat,w.vec)
  }
  return(w.mat)#p x m
  
  
  
  
}