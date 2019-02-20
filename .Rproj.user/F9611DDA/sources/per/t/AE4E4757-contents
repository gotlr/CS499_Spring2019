#' K nearest neighbors alogrithm
#'
#' An R function that wraps the nearest neighbor C++ code
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
#' zip.train[test.i]
knn <- function(x.mat, y.vec, testx.vec, max.neighbors )
{
  result.list <- .C("knn_interface",
                    as.double(x.mat), 
                    as.double(y.vec), 
                    as.double(testx.vec), 
                    as.integer(nrow(x.mat)), 
                    as.integer(ncol(x.mat)), 
                    as.integer(max.neighbors), 
                    predictions = double(max.neighbors), 
                    PACKAGE = "nearestNeighbors")
  result.list$predictions
}
