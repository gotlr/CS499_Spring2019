const int error_no_training_input=1;
const int error_no_training_output=2;
const int error_no_feature=3;
const int error_neighbors_is_small=4;

int NN1toKmaxPredict (
    const int n_train_observations,
    const int n_test_observations,
    const int n_features,
    const int max_neighbors,            // maximum number of neighbors
    const double *training_data_inputs, // a matrix training data inputs(n_train_observations x n_features)
    const double *training_data_outputs,// a vector of training data outputs (n_train_observations)
    const double *test_input_matrix,    // a test input matrix (n_test_observations x n_features)
    const double *test_data_prediction  // a matrix of predictions for the test data (n_test_observations x max_neighbors), which is where you need to store the result.
);