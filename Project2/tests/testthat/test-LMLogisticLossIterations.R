library(testthat)
library(linearModel)
data(SAheart, package = "ElemStatLearn")
X.mat <- data.matrix(subset(SAheart, select = -c(chd)))
y.vec <- as.vector(data.matrix(subset(SAheart, select = chd)))
max.iteration <- 500L
step.size <- 0.5L


test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    W.mat <-
      LMLogisticLossIterations(X.mat, y.vec, max.iteration, step.size)
    expect_true(is.numeric(W.mat))
    expect_true(is.matrix(W.mat))
    expect_equal(nrow(W.mat), ncol(X.mat))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      W.mat <- 
        LMLogisticLossIterations(as.data.frame(X.mat), y.vec, max.iteration, step.size),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossIterations(X.mat, y.vec[-1], max.iteration, step.size),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossIterations(X.mat, y.vec, as.double(max.iteration), step.size),
      "Input max.iterations must be a greater than 1 integer scalar number.",
      fixed = TRUE
    )
    expect_error(
      W.mat <-
        LMLogisticLossIterations(X.mat, y.vec, max.iteration, as.double(step.size)),
      "step.size must be a numeric scalar value.",
      fixed = TRUE
    )
  }
)