#include <Eigen/Dense>
#include "linear_models.h"

// LM__LossIterations Errors
  // Square Loss
int LMSquareLossIterations(
  const double *X_mat,
  const double *y_vec,
  const int n_observations,
  const int n_features,
  const int max_iterations,
  const double step_size,
  double *weight_mat
){
  // Matrix Access
  Eigen::Map< Eigen::MatrixXd > inputs_mat(
      (double*)X_mat,
      n_observations,
      n_features);
  Eigen::Map< Eigen::VectorXd > inputs_vec(
      (double*)X_mat,
      n_features);
  Eigen::VectorXd outcome_vec( n_observations );
  Eigen::VectorXi index_vec( n_observations );
  
  // Calculate Predictions
  for( int i = 0; i < n_observations; i++ ){
    // Some Code
    outcome_vec(i) = i; // Save outcomes
    index_vec(i) = i; // Save indices
  }
  
  // Sorting Outcomes
  std::sort(
    index_vec.data(),
    index_vec.data() + index_vec.size(),
    [&outcome_vec](int left, int right){
      return outcome_vec(left) < outcome_vec(right);
    }
  );
  
  // Adding to weight matrix
  for( int item = 0; item < max_iterations; item++ ){
    weight_mat[item] = outcome_vec[item];
  }
  
  return 0;
}

  // Logistic Loss
int LMLogisticLossIterations(
    const double *X_mat,
    const double *y_vec,
    const int n_observations,
    const int n_features,
    const int max_iterations,
    const double step_size,
    double *weight_mat
){
  // Matrix Access
  Eigen::Map< Eigen::MatrixXd > inputs_mat(
      (double*)X_mat,
      n_observations,
      n_features);
  Eigen::Map< Eigen::VectorXd > inputs_vec(
      (double*)X_mat,
      n_features);
  Eigen::VectorXd outcome_vec( n_observations );
  Eigen::VectorXi index_vec( n_observations );
  
  // Calculate Predictions
  for( int i = 0; i < n_observations; i++ ){
    // Some Code
    outcome_vec(i) = i; // Save outcomes
    index_vec(i) = i; // Save indices
  }
  
  // Sorting Outcomes
  std::sort(
    index_vec.data(),
    index_vec.data() + index_vec.size(),
    [&outcome_vec](int left, int right){
      return outcome_vec(left) < outcome_vec(right);
    }
  );
  
  // Adding to weight matrix
  for( int item = 0; item < max_iterations; item++ ){
    weight_mat[item] = outcome_vec[item];
  }
  
  return 0;
}


// LM__LossEarlyStoppingCV Errors
  // Square Loss Early Stopping
  void LMSquareLossEarlyStoppingCV(
      const double *X_mat,
      const double *y_vec,
      const int n_observations,
      const int n_features,
      const int max_iterations,
      const double *fold_vec, // Inputs
      double *weight_mat, // Outputs
      double *mean_validation_loss,
      double *mean_train_loss_vec,
      double *selected_steps,
      double *testXmat_vec_predictions
  ){
    // Code
  }
  
  // Logistic Loss Early Stopping
  void LMLogisticLossEarlyStoppingCV(
      const double *X_mat,
      const double *y_vec,
      const int n_observations,
      const int n_features,
      const int max_iterations,
      const double *fold_vec, // Inputs
      double *weight_mat, // Outputs
      double *mean_validation_loss,
      double *mean_train_loss_vec,
      double *selected_steps,
      double *testXmat_vec_predictions
  ){
    // Code
  }