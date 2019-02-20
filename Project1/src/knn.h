/*
 * From Screencast 2 - Cpp R interface
 */
int knn(
    const double *train_inputs_ptr,
    const double *train_label_ptr,
    const double *test_input_ptr,
    const int n_observations, 
    const int n_features,
    const int max_neighbors,
    double *test_predictions_ptr
);