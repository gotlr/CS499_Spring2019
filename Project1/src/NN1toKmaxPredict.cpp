#include <Eigen/Dense>
#include "NN1toKmaxPredict.h"
#include <omp.h> 
#include <iostream>

int NN1toKmaxPredict (
    const int n_train_observations,
    const int n_test_observations,
    const int n_features,
    const int max_neighbors,            // maximum number of neighbors
    const double *training_data_inputs, // a matrix training data inputs(n_train_observations x n_features)
    const double *training_data_outputs,// a vector of training data outputs (n_train_observations)
    const double *test_input_matrix,    // a test input matrix (n_test_observations x n_features)
    const double *test_data_prediction  // a matrix of predictions for the test data (n_test_observations x max_neighbors), which is where you need to store the result.
  ){
  
  if (n_train_observations == 0){return error_no_training_input;}
  if (n_test_observations == 0){return error_no_training_output;}
  if (n_features == 0){return error_no_feature;}
  if (max_neighbors == 0){return error_neighbors_is_small;}
  
  Eigen::Map< Eigen::MatrixXd > train_inputs_mat(
      (double*)training_data_inputs,
      n_train_observations,
      n_features);
  
  Eigen::Map< Eigen::VectorXd > test_input_vec(
      (double*)test_input_matrix,
      n_test_observations,
      n_features);
  
  Eigen::Map <Eigen::MatrixXd> test_predication_matrix(
      test_data_prediction, 
      n_test_observations, 
      max_neighbors);
  
  Eigen::VectorXd dist_vec(n_train_observations,n_test_observations); // Uses dynamic memory allocation
  
  Eigen::VectorXi sorted_index_vec(n_train_observations,n_test_observations); 
  
  Eigen::VectorXd dist_vec_2(n_train_observations);
  
  Eigen::VectorXi index_vec(n_train_observations);
  

  for (int test_index = 0; test_index < n_test_observations; test_index++)
  {
    for (int train_index = 0; train_index < n_train_observations; train_index++){
      
      dist_vec(test_index,train_index) = (train_inputs_mat.row(train.index)- test_input_vec.row(test_index)).norm();
    }}
  
  for (int test_index = 0; test_index < n_test_observations; test_index++)
  {
    for (int dist_index = 0; dist_index < n_train_observations; dist_index++){
      
      dist_vec_2(dist_index) = dist_vec(test_index, dist_index);
    }
    
    for(int index = 0 ;index < n_train_observations; index++){
      index_vec(index) = index;
    }
    
  std::sort(index_vec.data(), index_vec.data() + index_vec.size(),
            [&dist_vec_2](int lef, int right)
            {
              return dist_vec_2(left) < dist_vec_2(right);
            }
              
           );
    
    for (int index = 0; index < n_train_observations; index++)
    {
      sorted_index_vec(test_index, index) = index_vec(index);
    }
  }
  // Using Manhattan Distance => sum(xi-yi)
  // https://techblogmu.blogspot.com/2017/03/find-manhattan-distance-l1-norm-and.html
  for(int test_index = 0; test_index < n_test_observations; test_index++)
    double total = 0.0;
  for(int model_index=0; model_index < max_neighbors; model_index++)
  {
    int neighbors = model_i + 1;
    int row_index = sorted_index_vec(test_index, model_index);
    total+=training_data_outputs[row_index];
    test_data_prediction(test_index, model_index) = tital/neighbors;
    // int xi = sorted_index_vec(model_i);
    // int yi = test_input_vec(model_i);
    // total += abs(xi-yi);
  }
  
  return 0;
}
      

  