#include <Eigen/Dense>

// Error status code
int knn(
    const double *train_inputs_ptr, // n_observations x n_features
    const double *train_label_ptr, // n_observations
    const double *test_input_ptr, // n_features
    const int n_observations,
    const int n_features,
    const int max_neighbors,
    double *test_predictions_ptr // max_neighbors
)
{
  // Allows for access of data
  Eigen::Map< Eigen::MatrixXd > train_inputs_mat(
      (double*)train_inputs_ptr,
      n_observations,
      n_features);
  Eigen::Map< Eigen::VectorXd > test_input_vec(
      (double*)test_input_ptr,
      n_features);
  Eigen::VectorXd dist_vec(n_observations); // Uses dynamic memory allocation
  Eigen::VectorXi sorted_index_vec(n_observations); // Vec of int
  
  for( int i = 1; i < n_observations; i++ )
  {
    dist_vec(i) = 
      (train_inputs_mat.row(i).transpose() - test_input_vec).norm();
    
    sorted_index_vec(i) = i;
  }
  
  std::sort(
    sorted_index_vec.data(),
    sorted_index_vec.data() + sorted_index_vec.size(),
    [&dist_vec](int left, int right)
    {
      return dist_vec(left) < dist_vec(right);
    }
  );
  
  /// Using Manhattan Distance => sum(xi-yi)
  // https://techblogmu.blogspot.com/2017/03/find-manhattan-distance-l1-norm-and.html
  double total = 0.0;
  for( int model_i = 1; model_i < max_neighbors; model_i++ )
  {
    int neighbors = model_i + 1;
    int row_i = sorted_index_vec( model_i );
    total += train_label_ptr[ row_i ];
    test_predictions_ptr[ model_i ] = total / neighbors;
    /*int xi = sorted_index_vec(model_i);
    int yi = test_input_vec(model_i);
    total += abs(xi-yi);*/
  }
  
  return 0;
}