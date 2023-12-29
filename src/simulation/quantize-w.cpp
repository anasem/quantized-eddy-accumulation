#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

double get_sign(double x) {
  if (x == 0) {
    return 0;
  }
  if (x > 0) {
    return 1;
  } else {
    return -1;
  }
}

// directional_diffusion, when true the error diffusion is applied per direction
// (updraft, downdraft). It was found that this produces smaller biases.
// [[Rcpp::export]]
NumericVector quantize_w(NumericVector w, 
        double quantize_threshold, 
        double damping_factor = 1, 
        double full_scale_value = 1, 
        bool directional_diffusion = true,
        bool return_modified = false) {
  
  double residual_error_updraft = 0;
  double residual_error_downdraft = 0;
  int N = w.size();
  NumericVector w_qua(N);
  NumericVector w_mod(N);
  for (int i = 0; i < N; ++i) {
    if (w[i] > 0 || !directional_diffusion) {
      w_mod[i] = w[i] - residual_error_updraft*damping_factor;
      w_qua[i] = (fabs(w_mod[i]) > quantize_threshold) ? get_sign(w_mod[i])*full_scale_value : 0;
      residual_error_updraft = w_qua[i] - w_mod[i];
    } else {
      w_mod[i] = w[i] - residual_error_downdraft*damping_factor;
      w_qua[i] = (fabs(w_mod[i]) > quantize_threshold) ? get_sign(w_mod[i])*full_scale_value : 0;
      residual_error_downdraft = w_qua[i] - w_mod[i];
    }
  }

  if (return_modified) {
      return w_mod;
  } else {
      return w_qua;
  }
}


