# Reference simple implementation of the wind quantization with error diffusion
#
quantize_w_reference <- function(w, 
                                 quantize_threshold, 
                                 w_full_scale_value) {
    residual_error_updraft   <- 0
    residual_error_downdraft <- 0
    N <- length(w)
    w_qua <- rep(0, N)

    for (i in seq_along(w)) {
        # Error diffusion is carried out within the respective wind direction
        if (w[i] > 0) {
            # Add error from previous step to modified w 
            w_mod <- w[i] - residual_error_updraft 
            # Quantize modified wind
            w_qua[i] <- ifelse(abs(w_mod) > quantize_threshold, 
                               sign(w_mod)*w_full_scale_value, 0)
            # Calculate residual error
            residual_error_updraft <- w_qua[i] - w_mod
        } else {
            w_mod <- w[i] - residual_error_downdraft
            w_qua[i] <- ifelse(abs(w_mod) > quantize_threshold, 
                               sign(w_mod)*w_full_scale_value, 0)
            residual_error_downdraft <- w_qua[i] - w_mod
        }
        
    }
    return(w_qua)
}


