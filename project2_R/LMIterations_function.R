LMSquareLossIterations <-
  function(X.mat, y.vec, max.iterations, step.size = 0.001) {
    if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
      stop("X.mat must be a numeric matrix.")
    }
    
    if (!all(is.vector(y.vec),
             is.numeric(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
    }
    
    if (!all(is.integer(max.iterations),
             max.iterations > 1,
             length(max.iterations) == 1)) {
      stop("Input max.iterations must be a greater than 1 integer scalar number.")
    }
    
    if (!all(is.numeric(step.size), length(step.size) == 1)) {
      stop("step.size must be a numeric scalar value.")
    }
    
    num.train <- dim(X.mat)[1]
    num.feature <- dim(X.mat)[2]
    W=matrix(data=0,ncol=1,nrow=num.feature)
    W.mat=matrix(0,ncol=max.iterations,nrow=num.feature)
    X.scale.vec <- scale(X.mat,center=TRUE,scale = TRUE)[1:num.train,1:num.feature]#variance=1,mean=0
    
    
    # for-loop to get the slope.mat matrix
    for (iter.index in (1:max.iterations)) {
      grad <- t(X.scale.vec) %*% (X.scale.vec %*% W -  y.vec);
        if (sqrt(as.numeric(t(grad) %*% grad)) < 1e-6){
          println(sprintf('iter times=%d', iter.index));
          break;

    }
    W <- W - grad * step.size;
    W.mat[,iter.index]=W
      }
    return(W.mat)
      
    }








LMLogisticLossIterations <-
  function(X.mat, y.vec,max.iterations, step.size = 0.05) {
    if (!all(is.matrix(X.mat), is.numeric(X.mat))) {
      stop("X.mat must be a numeric matrix.")
    }

    if (!all(is.vector(y.vec),
             is.numeric(y.vec),
             length(y.vec) == nrow(X.mat))) {
      stop("y.vec must be a numeric vector of the same number of rows as X.mat.")
    }

    if (!all(is.integer(max.iterations),
             max.iterations > 1,
             length(max.iterations) == 1)) {
      stop("Input max.iterations must be a greater than 1 integer scalar number.")
    }

    if (!all(is.numeric(step.size), length(step.size) == 1)) {
      stop("step.size must be a numeric scalar value.")
    }
    num.train <- dim(X.mat)[1]
    num.feature <- dim(X.mat)[2]
    W=matrix(data=0,ncol=1,nrow=num.feature)
    sigmoid <- function(z) { 1 / (1 + exp(-z))}
    X.scale.vec <- scale(X.mat,center=TRUE,scale = TRUE)[1:num.train,1:num.feature]#variance=1,mean=0
    W.mat=matrix(0,ncol=max.iterations,nrow=num.feature)
    for (i in 1:max.iterations){
      grad <- t(X.scale.vec) %*% (sigmoid(X.scale.vec %*% W)-y.vec);
      if (sqrt(as.numeric(t(grad) %*% grad)) < 1e-6){
        print(sprintf('iter times=%d', i));
        break;
      }
      W <- W - grad * step.size;
      W.mat[,i]=W
    }
    return(W.mat)
  }