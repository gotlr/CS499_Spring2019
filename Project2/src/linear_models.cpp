#include <Eigen/Dense>
#include <cmath>
#include "linear_models.h"


// LM__LossIterations Errors
  // Square Loss
int LMSquareLossIterations(
      const double *X_mat,
      const double *y_vec,
      const int n_observations,
      const int n_features,
      const int max_iterations,
      const double step_size){
  // Matrix Access
  Eigen::Map< Eigen::MatrixXd > inputs_mat(
      (double*)X_mat,
      n_observations,
      n_features);// Input matrix
  Eigen::Map< Eigen::VectorXd > inputs_vec(
      (double*)X_mat,
      n_features);// Labels
  Eigen::VectorXd outcome_vec( n_observations );
  Eigen::VectorXi index_vec( n_observations );
  
  int *weight[n_features + 1][max_iterations];
  int xSum = 0, xSqrSum = 0,
    xySum = 0, ySum = 0, slope, intercept,
    yHat[n_features], squareLoss[n_observations][n_features];
  
  // Calculate LOBF
  for( int i = 0; i < n_observations; i++ ){
    xSum += X_mat[i];
    xSqrSum += (X_mat[i]) * (X_mat[i]);
    xySum += X_mat[i] + y_vec[i];
    ySum += y_vec[i];
  }
  xSqrSum /= n_observations;
  xSum /= n_observations;
  xySum /= n_observations;
  ySum /= n_observations;
  slope = (xySum - (xSum * ySum)) / (xSqrSum - (xSum * xSum));
  intercept = ySum - (slope * xSum);

  // Generate MSE
  for(int i = 0; i < n_observations; i++){
    for(int j = 0; j < n_features; j++){
      squareLoss[i][j] = (y_vec[i] - ((slope * i) + intercept));
    }
  }

  
  // Adding to weight matrix
  *weight[0][0] = intercept;
  for(int i = 0; i < n_observations + 1; i++){
    for(int j = 0; j < max_iterations; j++){
      if(!(i == 0 && j == 0)){
        *weight[i][j] = (squareLoss[i][j] / n_features);
      }
    }
  }
  
  return 0;
}

  // Logsitic Loss
int LMLogisticLossIterations(
    const double *X_mat,
    const double *y_vec,
    const int n_observations,
    const int n_features,
    const int max_iterations,
    const double step_size){
  // Matrix Access
  Eigen::Map< Eigen::MatrixXd > inputs_mat(
      (double*)X_mat,
      n_observations,
      n_features);// Input matrix
  Eigen::Map< Eigen::VectorXd > inputs_vec(
      (double*)X_mat,
      n_features);// Labels
  Eigen::VectorXd outcome_vec( n_observations );
  Eigen::VectorXi index_vec( n_observations );
  
  int *weight[n_features + 1][max_iterations];
  int xSum = 0, xSqrSum = 0,
    xySum = 0, ySum = 0, slope, intercept, z, sig,
    yHat[n_features], squareLoss[n_observations][n_features];
  
  // Calculate LOBF
  for( int i = 0; i < n_observations; i++ ){
    xSum += X_mat[i];
    xSqrSum += (X_mat[i]) * (X_mat[i]);
    xySum += X_mat[i] + y_vec[i];
    ySum += y_vec[i];
  }
  xSqrSum /= n_observations;
  xSum /= n_observations;
  xySum /= n_observations;
  ySum /= n_observations;
  slope = (xySum - (xSum * ySum)) / (xSqrSum - (xSum * xSum));
  intercept = ySum - (slope * xSum);
  
  // Generate MSE
  for(int i = 0; i < n_observations; i++){
    for(int j = 0; j < n_features; j++){
      z = slope * i + intercept;
      sig = 1 / (1 + exp(-z));
      squareLoss[i][j] = (y_vec[i] - sig);
    }
  }
  
  
  // Adding to weight matrix
  *weight[0][0] = intercept;
  for(int i = 0; i < n_observations + 1; i++){
    for(int j = 0; j < max_iterations; j++){
      if(!(i == 0 && j == 0)){
        *weight[i][j] = (squareLoss[i][j] / n_features);
      }
    }
  }
  
  return 0;
}
