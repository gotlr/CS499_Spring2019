/*
 * From Screencast 2 - Cpp R interface
 */

#include "knn.h"
#include <R.h>
#include <R_ext/Rdynload.h>
#include "mymean.h"

void my_mean_interface(int *data_ptr, int *data_count, double *output_ptr){
  int status = my_mean_C(data_ptr, *data_count, output_ptr);
  if(status == MY_MEAN_ERROR_NO_DATA){
    error("no data");
  }
  if(status != 0){
    error("unrecognized error ", status);
  }
}

void knn_interface(
    const double *train_inputs_ptr,
    const double *train_label_ptr,
    const double *test_input_ptr,
    const int *n_observations_ptr, 
    const int *n_features_ptr,
    const int *max_neighbors_ptr,
    double *test_predictions_ptr)
  {
  int status = knn(train_inputs_ptr,
                   train_label_ptr, 
                   test_input_ptr,
                   *n_observations_ptr,
                   *n_features_ptr, 
                   *max_neighbors_ptr,
                   test_predictions_ptr);
  
  if(status != 0)
    {
      error("non-zero exit status from knn");
    }
  }

R_CMethodDef cMethods[] = {
  {"knn_interface", (DL_FUNC) &knn_interface, 7},
  {"my_mean_interface", (DL_FUNC) &my_mean_interface, 3},
  {NULL, NULL, 0}
};

extern "C"{
  void R_init_nearestNeighbors(DllInfo *info){
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
  }
}
