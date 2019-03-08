library(testthat)
library(linearModel)
data(SAheart, package = "ElemStatLearn")
X.mat <- data.matrix(subset(SAheart, select = -c(chd)))
y.vec <- as.vector(data.matrix(subset(SAheart, select = chd)))
fold.vec <- as.vector(data.matrix(subset(SAheart, select =  chd)))
max.iteration <- 500L


test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    result.list <-
      LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iteration)
    expect_true(is.list(result.list))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      result.list <- 
        LMLogisticLossEarlyStoppingCV(as.data.frame(X.mat), y.vec, fold.vec, max.iteration),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossEarlyStoppingCV(X.mat, y.vec[-1], fold.vec, max.iteration),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec[-1], max.iteration),
      "fold.vec must be assigned before input and it must be a integer vector",
      fixed = TRUE
    )
    expect_error(
      result.list <-
        LMLogisticLossEarlyStoppingCV(X.mat, y.vec, fold.vec, as.double(max.iteration)),
      "max.iterations must be an integer scalar greater than zero",
      fixed = TRUE
    )
  }
)
