#' K nearest neighbors alogrithm
#'
#' An R function that wraps the nearest neighbor C++ code
#' 
#' @param x.mat numeric train feature matrix [ n x p ]
#' @param y.vec numeric train label vector [n], either all 
#' 0/1 for binary classification, or other real numbers for
#' regression (multi-class classification not supported).
#' @param testx.vec numeric test feature vector [p]
#' @param max.neighbors scalar integer, max number of neighbors
#'
#' @return numeric vector of size max.neighbors, predictions
#' from 1 to max.neighbors.
#' @export
#'
#' @examples
#' data(zip.train, package = "ElemStatLearn")
#' i01 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- i01[1:5]
#' test.i <- i01[6]
#' x <- zip.train[train.i, -1]
#' y <- zip.train[train.i, 1]
#' testx <- zip.train[test.i, -1]
#' knn(x, y, testx, 3)
#' zip.train[test.i, 1]

#  If your function returns an error code, make sure to call error() with an informative message that will be displayed in R.
NN1toKmaxPredict(X.mat, y.vec, testX.mat, max.neighbors){
  if(!all(is.matrix(X.mat),is.numeric(X.mat))){
    stop("X.mat must be a numeric matrix")
  }
  if(!all(is.numeric(y.vec), is.vector(y.vec),length(y.vec) == nrow(X.mat))){
    stop("y.vec must be a numeric vector of size(X.mat)")
  }
  if(!all(is.numeric(testX.mat), is.matrix(testX.mat), ncol(testX.mat) == ncol(X.mat))){
    stop("testX.mat must be a numeric matrix with ncol(X.mat) columns")
  }
  if(!all(is.integer(max.neighbors), length(max.neighbors) == 1)){
    stop("max.neighbors must be an integer scalar")
  }
#  calls your C++ code via the .C function in R and return a list
   list <- .C(
    "NN1toKmaxPredict_interface",
    as.integer(nrow(X.mat)), 
    as.integer(nrow(testX.mat)), 
    as.integer(ncol(X.mat)), 
    as.integer(max.neighbors), 
    as.double(X.mat), 
    as.double(y.vec), 
    as.double(testX.mat),
    prediction = as.double(matrix(rep(0,nrow(testX.mat)*max.neighbors), nrow = nrow(testX.mat))),
    PACKAGE = "nearestNeighbors"
  )
   list$prediction = matrix(list$prediction,ncol=max.neighbors)
  return(list$prediction)
}



#' Cross-validation using nearest neighbors
#'
#' An R function that wraps the nearest neighbor C++ code
#' 
#' @param x.mat numeric train feature matrix [ n x p ]
#' @param y.vec numeric train label vector [n], either all 
#' 0/1 for binary classification, or other real numbers for
#' regression (multi-class classification not supported).
#' @param testx.vec numeric test feature vector [p]
#' @param max.neighbors scalar integer, max number of neighbors
#'
#' @return numeric vector of size max.neighbors, predictions
#' from 1 to max.neighbors.
#' @export
#'
#' @examples
#' data(zip.train, package = "ElemStatLearn")
#' i01 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- i01[1:5]
#' test.i <- i01[6]
#' x <- zip.train[train.i, -1]
#' y <- zip.train[train.i, 1]
#' testx <- zip.train[test.i, -1]
#' knn(x, y, testx, 3)
#' zip.train[test.i, 1]
#' 

NNLearnCV(X.mat, y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5)
{
  #write type/dimension checking code in the beginning of that function (make sure that fold.vec is the same size as y.vec, which is the same as the number of rows in X.mat). Use stop() with an informative error message if there are any issues.
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
  
  if (!all(is.integer(n.folds), length(n.folds) == 1)) {
    stop("n.folds must be an integer scalar")
  }
  
  fold.vec=unique(fold.vec)
  max.fold=length(fold.vec)
  label.is.binary = (y.vec == 0) || (y.vec == 1)
  fold.split=sample(1:max.fold,max.fold,replace=F)
  #max.fold <- length(fold.vec)
  validation.loss.mat <-matrix(0, max.fold, max.iterations)
  train.loss.mat <- matrix(0, max.fold, max.iterations)#initial the condition
  
  for (i in max.fold){
    train.index <- which( fold.split !=i)
    validation.index <- which( fold.split == i)
    CV.result <-
      NN1toKmaxPredict(X.mat[train.index,], y.vec[train.index], X.mat[validation.index], max.neighbors)
    
    CV.result <-
      matrix(CV.result, ncol = max.neighbors)
    loss.mat <- if (label.is.binary) {
      ifelse(CV.result > 0.5, 1, 0) != y.vec[validation.index]
    } else{
      (CV.result - y.vec[validation.index]) ^ 2
    }
    
    train.loss.vec <- rowMeans(train.loss.mat)
    validation.loss.vec <- rowMeans(validation.loss.mat)
    selected.neighbors <- which.min(validation.loss.vec)
    
    predict <- function(testX.mat) {      
      #a function that takes a matrix of inputs/features and returns a vector of predictions. It should check the type/dimension of testX.mat and stop() with an informative error message if there are any issues.
      if (!all(is.numeric(testX.mat),
               is.matrix(testX.mat),
               ncol(testX.mat) == ncol(X.mat))) {
        stop("testX.mat must be a numeric matrix with ncol(X.mat) columns")
      }
      
      result <-
        NN1toKmaxPredict(X.mat, y.vec, testX.mat, as.integer(selected.neighbors))
      prediction.vec <-
        prediction.result[, selected.neighbors]
      if (label.is.binary){
        prediction.vec <- ifelse(prediction.vec > 0.5, 1, 0)
      }
      return(prediction.vec)
    }
    
    return.list <-
      list(
        X.mat = X.mat,
        y.vec = y.vec,
        train.loss.mat = train.loss.mat,
        validation.loss.mat = validation.loss.mat,
        train.loss.vec = rowMeans  (train.loss.mat),
        validation.loss.vec = rowMeans(validation.loss.mat),
        selected.neighbors = which.min(validation.loss.vec),
        predict = predict
      )
  }
  
}

