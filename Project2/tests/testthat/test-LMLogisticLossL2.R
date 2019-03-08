library(testthat)
library(linearModel)
data(SAheart, package = "ElemStatLearn")
X.mat <- data.matrix(subset(SAheart, select = -c(chd)))
y.vec <- as.vector(data.matrix(subset(SAheart, select = chd)))
penalty <- 3L
opt.thresh <- 0.001L
initial.weight.vec <- as.vector(data.matrix(subset(SAheart, select = chd)))


test_that(
  "For valid inputs, your function returns an output of the expected type/dimension",
  {
    opt.weight.vec <-
      LMLogisticLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec)
    expect_true(is.vector(opt.weight.vec))
  }
)

test_that(
  "For an invalid input, your function stops with an informative error message.",
  {
    expect_error(
      opt.weight.vec <- 
        LMLogisticLossL2(as.data.frame(X.scaled.mat), y.vec, penalty, opt.thresh, initial.weight.vec),
      "X.mat must be a numeric matrix.",
      fixed = TRUE
    )
    expect_error(
      opt.weight.vec <-
        LMLogisticLossL2(X.scaled.mat, y.vec[-1], penalty, opt.thresh, initial.weight.vec),
      "y.vec must be a numeric vector of the same number of rows as X.mat.",
      fixed = TRUE
    )
    expect_error(
      opt.weight.vec <-
        LMLogisticLossL2(X.scaled.mat, y.vec, as.double(penalty), opt.thresh, initial.weight.vec),
      "penalty must be a non-negative numeric scalar.",
      fixed = TRUE
    )    
    expect_error(
      opt.weight.vec <-
        LMLogisticLossL2(X.scaled.mat, y.vec, penalty, as.double(opt.thresh), initial.weight.vec),
      "opt.thresh must be a positive numeric scalar.",
      fixed = TRUE
    )    
    expect_error(
      opt.weight.vec <-
        LMLogisticLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec[-1]),
      "initial.weight.vec must be a numeric vector.",
      fixed = TRUE
    )
  }
)
