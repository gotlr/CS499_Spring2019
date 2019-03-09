#' Linear model L2 regularization with squareloss
#' @param x.scaled.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric matrix of length nrow(x.scaled.mat)
#' @param penalty a non-negative numeric scalar
#' @param opt.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size ncol(x.scaled.mat) 
#' @return opt.weight the optimal weight vector of length ncol(X.scaled)



LMSquareLossL2 <-function(X.scaled.mat,y.vec,penalty,opt.thresh,initial.weight.vec) {
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
    
    if (!all(length(opt.thresh) == 1,is.numeric(opt.thresh),opt.thresh > 0)) {
      stop("opt.thresh must be a positive numeric scalar.")
    }
    
    if (!all(is.vector(initial.weight.vec),is.numeric(initial.weight.vec))) {
      stop("initial.weight.vec must be a numeric vector.")
    }
    
    weight.vec <- initial.weight.vec
    # Compute the gradient
    while (1) {
      loss <- 2 * t(X.scaled.mat) %*%(X.scaled.mat %*% weight.vec - y.vec) + 2 * penalty * weight.vec
      
      if (sum((abs(loss))^2) <= opt.thresh) {
        break
      } 
      else {loss
        weight.vec <- weight.vec - penalty * loss
      }
      
    }
    optimal.weight <- weight.vec
    return(optimal.weight)
    
}

#' Linear model L2 regularization with logisticloss
#' @param x.scaled.mat a numeric matrix of size [n x p]
#' @param y.vec a numeric matrix of length nrow(x.scaled.mat)
#' @param penalty a non-negative numeric scalar
#' @param opt.thresh a positive numeric scalar
#' @param initial.weight.vec a numeric vector of size ncol(x.scaled.mat) 
#' @return opt.weight the optimal weight vector of length ncol(X.scaled)


LMLogisticLossL2 <-function(X.scaled.mat,y.vec,penalty,opt.thresh = 0.01,initial.weight.vec) {
  if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
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
  
  if (!all(length(opt.thresh) == 1,
           is.numeric(opt.thresh),
           opt.thresh > 0)) {
    stop("opt.thresh must be a positive numeric scalar.")
  }
  
  if (!all(is.vector(initial.weight.vec),
           is.numeric(initial.weight.vec))) {
    stop("initial.weight.vec must be a numeric vector.")
  }
  
  weight.vec <- initial.weight.vec
  sigmoid <- function(z) { 1 / (1 + exp(-z))}
  # Compute the gradient
  while (1) {
    loss <- t(x.scaled) %*% (sigmoid(x.scaled %*% w.mat[, dim(w.mat)[2]] )-y.vec) + 2 * penalty * weight.vec
    
    if (sum((abs(loss))^2) <= opt.thresh) {
      break
    } 
    else {
      weight.vec <- weight.vec - penalty * loss
    }
    
  }
  optimal.weight <- weight.vec
  return(optimal.weight)
  
}

