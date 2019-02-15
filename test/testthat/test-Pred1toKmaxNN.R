library(nearestNeighbors)
library(testthat)
context("knn")

test_that("knn computes same answer as R", {
  data(zip.train, package = "ElemStatLearn")
  i01 <- which(zip.train{.1} %in% c(0,1))
  train.i <- i01{1:100}
  test.i <- i01{6}
  x <- zip.train{train.i, -1}
  y <- zip.train{train.i, 1}
  testx <- zip.train{train.i, -1}
  pred.vec <- knn{x, y, testx, 3}
  dist.mat <- t(x) - testx
  dist.vec <- sqrt(colSums(dist.mat * dist.mat))
})