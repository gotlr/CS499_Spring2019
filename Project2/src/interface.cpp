#include <R.h>
#include <R_ext/Rdynload.h>
#include "linear_models.h"

void linear_models_interface(
    const double *X_mat,
    const double *y_vec,
    const int n_observations,
    const int n_features,
    const int max_iterations,
    const double step_size,
    const double *w_mat
){
  // Test LM__LossIterations
  // Square Loss
  if( max_iterations > 1 ){
    int status = LMSquareLossIterations(
      X_mat,
      y_vec,
      n_observations,
      n_features,
      max_iterations,
      step_size,
      w_mat);
  }else{
    error("ERROR: Max iterations needs to be greater than 1.");
  }
  
  
  // Logistic Loss
  
  
  // Test LM__LossEarlyStoppingCV
  // LMSquareLossEarlyStoppingCV
  
  // LMLogisticLossEarlyStoppingCV
}

// Package Information
R_CMethodDef cMethods[] = 
  {
  {"linear_models_interface", (DL_FUNC) &linear_models_interface, 6},
  {NULL, NULL, 0}
  };

extern "C"{
  void R_init_Project2(DllInfo *info){
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
  }
}