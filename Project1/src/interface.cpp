#include "NN1toKmaxPredict.h"
#include <R.h>
#include <R_ext/Rdynload.h>
#include <iostream>
using namespace std ;



//write a C++ function NN1toKmaxPredict which takes a training data set and an entire test matrix, 
//then computes a matrix of k-nearest neighbor predictions, for k=1 to max_neighbors, 
//and for every test observation. 
void NN1toKmaxPredict_interface (
    const int n_train_observations,
    const int n_test_observations,
    const int n_features,
    const int max_neighbors,
    const double *training_data_inputs,
    const double *training_data_outputs,
    const double *test_input_matrix,
    const double *test_data_prediction){
  
  int status = NN1toKmaxPredict(
     n_train_observations,
     n_test_observations,
     n_features,
     max_neighbors,            
     training_data_inputs, 
     training_data_outputs,
     test_input_matrix,    
     test_data_prediction );
     
  if(status == error_no_training_input){
    cout << "No test data";}
  
  if(status == error_no_training_output ){
    cout << "No test data";}
  
  if(status == error_no_feature ){
    cout << "feature is not enough";}
  
  if(status == error_neighbors_is_small ){
    cout << "number of max_neighbors must greater than 0";}
  
  R_CMethodDef cMethods[] = {
    {"NN1toKmaxPredict_interface",(DL_FUNC) &NN1toKmaxPredict_interface,7},
    {NULL,NULL,0}
  };
  
  extern "C"{
    void R_init_nearestNeighbors(DllInfo *info){
      R_registerRoutines(info, cMethods, NULL, NULL, NULL);
      R_useDynamicSymbols(info, FALSE);
    }


  
  
}