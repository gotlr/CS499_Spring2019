#' Title
#'
#' @param x.mat 
#' @param y.vec 
#' @param testx.vec 
#' @param max.neighbors 
#'
#' @return
#' @export
#'
#' @examples
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