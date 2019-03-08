LMSquareLossEarlyStoppingCV <-
  function(x.mat, y.vec, fold.vec, max.iteration){
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
    
    if (!all(is.integer(fold.vec), is.vector(fold.vec))) {
      stop("fold.vec must be assigned before input and it must be a integer vector")
    }
    #fold.number=4
    #fold.split=sample(1:fold.number,length(fold.vec),replace=T)
    max.fold <- length(fold.vec)
    validation.loss.mat <-matrix(0, fold.number, max.iteration)
    train.loss.mat <- matrix(0, n.folds, max.iteration)#initial the condition
    
    for (i in max.fold){
      train.index <- which(fold.split !=i)
      validation.index <- which(fold.split == i)
      w.mat <-LMSquareLossIterations(x.mat[train.index, ], y.vec[train.index, ], max.iterations,step.size = 0.5)
      train.loss <- ((x.mat[train.index, ] %*% w.mat)-y.vec[train.index, ])^2
      validation.loss <- ((x.mat[validation.index, ] %*% w.mat)-y.vec[train.index, ])^2
      mean.train.loss.vec <- colMeans(train.loss)
      mean.validation.loss.vec <- colMeans(validation.loss)
      train.loss.mat[fold.number, ] = mean.train.loss.vec
      validation.loss.mat[fold.number, ] = mean.validation.loss.vec
      
    }
    
    mean.train.loss.vec <- colMeans(train.loss.mat)
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    selected.steps <- which.min(mean.validation.loss.vec)#find the min value of each iteration
    W.mat <- LMSquareLossIterations(X.mat, y.vec, selected.steps)#get the desire max.iteration
    weight.vec <- W.mat[,selected.steps]#the last col of the W.mat(The best one)
    predict <- function(testX.mat){
      #a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
      
      prediction.vec <- testX.mat %*% weight.vec
      
    }
    Output <-#Output a list with the following named elements
      list(
        mean.validation.loss.vec = mean.validation.loss.vec,
        mean.train.loss.vec = mean.train.loss.vec,
        selected.steps = selected.steps,
        weight.vec = weight.vec,
        predict = predict
      )
    return(Output)
  }

    
  


LMLogisticLossEarlyStoppingCV <-
  function(x.mat, y.vec, fold.vec, max.iteration){
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
    
    if (!all(is.integer(fold.vec), is.vector(fold.vec))) {
      stop("fold.vec must be assigned before input and it must be a integer vector")
    }
    fold.number=4
    fold.split=sample(1:fold.number,length(fold.vec),replace=T)
    validation.loss.mat <-matrix(0, fold.number, max.iteration)
    train.loss.mat <- matrix(0, n.folds, max.iteration)#initial the condition
    
    for (i in fold.number){
      train.index <- which(fold.split !=i)
      validation.index <- which(fold.split == i)
      w.mat <-LMSquareLossIterations(x.mat[train.index, ], y.vec[train.index, ], max.iterations,step.size = 0.5)
      train.loss <- ((x.mat[train.index, ] %*% w.mat)-y.vec[train.index, ])^2
      validation.loss <- ((x.mat[validation.index, ] %*% w.mat)-y.vec[train.index, ])^2
      mean.train.loss.vec <- colMeans(train.loss)
      mean.validation.loss.vec <- colMeans(validation.loss)
      train.loss.mat[fold.i, ] = mean.train.loss.vec
      validation.loss.mat[fold.i, ] = mean.validation.loss.vec
      
    }
    
    mean.train.loss.vec <- colMeans(train.loss.mat)
    mean.validation.loss.vec <- colMeans(validation.loss.mat)
    selected.steps <- which.min(mean.validation.loss.vec)#find the min value of each iteration
    W.mat <- LMLogisticLossIterations(X.mat, y.vec, selected.steps)#get the desire max.iteration
    weight.vec <- W.mat[,selected.steps]#the last col of the W.mat(The best one)
    predict <- function(testX.mat){
      #a function that takes a test features matrix and returns a vector of predictions (real numbers for regression, probabilities for binary classification).
      
      prediction.vec <- testX.mat %*% weight.vec
      
    }
    Output <-#Output a list with the following named elements
      list(
        mean.validation.loss.vec = mean.validation.loss.vec,
        mean.train.loss.vec = mean.train.loss.vec,
        selected.steps = selected.steps,
        weight.vec = weight.vec,
        predict = predict
      )
    return(Output)
  }


}