library(testthat)
library(linearModel)
data(SAheart, package = "ElemStatLearn")
X.mat <- data.matrix(subset(SAheart, select = -c(chd)))
y.vec <- as.vector(data.matrix(subset(SAheart, select = chd)))
fold.vec <- as.vector(data.matrix(subset(SAheart, select =  chd)))
penalty.vec <- as.vector(data.matrix(subset(SAheart, select =  chd)))


test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    result.list <-
      LMLogisticLossL2CV(X.mat, y.vec, fold.vec, penalty.vec)
    expect_true(is.list(result.list))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      result.list <- 
        LMLogisticLossL2CV(as.data.frame(X.mat), y.vec, fold.vec, penalty.vec),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossL2CV(X.mat, y.vec[-1], fold.vec, penalty.vec),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossL2CV(X.mat, y.vec, fold.vec[-1], penalty.vec),
      "fold.vec must be assigned before input and it must be a integer vector",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossL2CV(X.mat, y.vec, fold.vec, penalty.vec[-1]),
      "penalty.vec must be a non-negative decreasing numeric vector",
      fixed = TRUE
    )
  }
)

