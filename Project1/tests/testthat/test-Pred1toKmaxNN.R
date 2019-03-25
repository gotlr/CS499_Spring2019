library(testthat)
library(NearestNeighbors)
data(prostate, package = "ElemStatLearn")
X.mat <- data.matrix(subset(prostate, select = -c(train, lpsa)))
y.vec <- as.vector(data.matrix(subset(prostate, select = lpsa)))
testX.mat <- X.mat[1:5,]
max.neighbors <- 3L

test_that(
  "For a valid input, NN1toKmaxPredict returns a valid numeric prediction matrix with the expected dimensions",
  {
    C.pred.mat <-
      NN1toKmaxPredict(X.mat, y.vec, testX.mat, max.neighbors)
    expect_true(is.numeric(C.pred.mat))
    expect_equal(nrow(C.pred.mat), nrow(testX.mat))
  }
)

test_that("For an invalid input, NN1toKmaxPredict stops with an informative error message",
          {
            expect_error(
              C.pred.mat <-
                NN1toKmaxPredict(as.data.frame(X.mat), y.vec, testX.mat, max.neighbors),
              "X.mat must be a numeric matrix",
              fixed = TRUE
            )
            expect_error(
              C.pred.mat <-
                NN1toKmaxPredict(X.mat, y.vec[-1], testX.mat, max.neighbors),
              "y.vec must be a numeric vector of size(X.mat)",
              fixed = TRUE
            )
            expect_error(
              C.pred.mat <-
                NN1toKmaxPredict(X.mat, y.vec, testX.mat[, -1], max.neighbors),
              "testX.mat must be a numeric matrix with nrcol(X.mat) columns",
              fixed = TRUE
            )
            expect_error(
              C.pred.mat <-
                NN1toKmaxPredict(X.mat, y.vec, testX.mat, as.double(max.neighbors)),
              "max.neighbors must be an integer scalar",
              fixed = TRUE
            )
          })
