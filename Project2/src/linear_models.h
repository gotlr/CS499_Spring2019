int LMSquareLossIterations(
    const double *X_mat,
    const double *y_vec,
    const int n_observations,
    const int n_features,
    const int max_iterations,
    const double step_size,
    double *weight_mat
);

int LMLogisticLossIterations(
    const double *X_mat,
    const double *y_vec,
    const int n_observations,
    const int n_features,
    const int max_iterations,
    const double step_size,
    double *weight_mat
);

void LMSquareLossEarlyStoppingCV(
    const double *X_mat,
    const double *y_vec,
    const int n_observations,
    const int n_features,
    const int max_iterations,
    const double *fold_vec, 
    double *weight_mat, 
    double *mean_validation_loss,
    double *mean_train_loss_vec,
    double *selected_steps,
    double *testXmat_vec_predictions
);

void LMLogisticLossEarlyStoppingCV(
    const double *X_mat,
    const double *y_vec,
    const int n_observations,
    const int n_features,
    const int max_iterations,
    const double *fold_vec,
    double *weight_mat,
    double *mean_validation_loss,
    double *mean_train_loss_vec,
    double *selected_steps,
    double *testXmat_vec_predictions
);