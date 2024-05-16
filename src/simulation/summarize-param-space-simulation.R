# Read and summarize results

source("src/simulation/deps.R")
source("src/config/default.R")

# Read simulation reults -------------------------------------------------------

message("Reading results files")

sim_param_spc  <- readRDS(sim_param_spc_fn)

# ------------------------------------------------------------------------------
# Error summary for parameter space simulation
message("Summarizing errors")

sim_errors <- 
    rbind(sim_param_spc[, c(error_summary(fe_co2_q, co2_flux), 
                            list(u_norm = sd(ndfe_co2_q)),
                            scalar = "co2"), 
              by = .(full_scale_value, qthreshold, directional_diffusion, station)],
          sim_param_spc[, c(error_summary(fe_h2o_q, h2o_flux), 
                            list(u_norm = sd(ndfe_h2o_q)),
                            scalar = "h2o"), 
              by = .(full_scale_value, qthreshold, directional_diffusion, station)],
          sim_param_spc[, c(error_summary(fe_ts_q, ts_flux), 
                            list(u_norm = sd(ndfe_Ts_q)),
                            scalar = "ts"), 
              by = .(full_scale_value, qthreshold, directional_diffusion, station)]
          )

message("Calculating means")
# Means by treatment
sim_means <- 
    sim_param_spc[, lapply(.SD, mean, na.rm = TRUE), 
                       by = .(full_scale_value, qthreshold, 
                              directional_diffusion, station, treatment_id)]

# ------------------------------------------------------------------------------
seconds_per_ai <- 1800

nzero_w_err_dt <- 
    sim_param_spc[
              , .(nzw_err_mean = mean(alpha_co2_q*w_mean/
                                      wq_abs_mean*co2_flux_q, na.rm = TRUE),
                  nzw_err_ref = mean(alpha_co2*w_mean/
                                     w_abs_mean*co2_flux, na.rm = TRUE),
                  nzw_err_sd = sd(alpha_co2_q*w_mean/
                                  wq_abs_mean*co2_flux_q, na.rm = TRUE),
                  nzw_err_sd_rel = sd(alpha_co2_q*w_mean/
                                      wq_abs_mean*co2_flux_q, na.rm = TRUE)/
                      sd(co2_flux_q, na.rm = TRUE),
                  nzw_err_rel = mean(alpha_co2_q*w_mean/
                                     wq_abs_mean*co2_flux_q, na.rm = TRUE)/
                      mean(co2_flux_q, na.rm = TRUE),
                  reduction_ratio = mean(w_mean/
                                         wq_abs_mean*co2_flux_q, na.rm = TRUE),
                  alpha_co2_q = mean(alpha_co2_q, na.rm = TRUE),
                  open_rate_hz = mean(N_q/seconds_per_ai, na.rm = TRUE),
                  open_rate_hz_sd = sd(N_q/seconds_per_ai, na.rm = TRUE),
                  open_rate_up_hz = mean(N_up_q/seconds_per_ai, na.rm = TRUE),
                  open_rate_down_hz = mean(N_down_q/seconds_per_ai, na.rm = TRUE),
                  alpha_mean_rel = mean(alpha_co2_q/alpha_co2, na.rm = TRUE),
                  w_emp_ratio = mean(wq_abs_mean)/ 
                      mean(w_abs_mean, na.rm = TRUE),
                  w_red_ratio = mean(w_abs_mean/wq_abs_mean, na.rm = TRUE),
                  C_emp_ratio = mean(abs(co2_delta_q), na.rm = TRUE)/
                                     mean(abs(co2_delta), na.rm = TRUE),
                  C_emp_ratio.h2o = mean(h2o_delta_q, na.rm = TRUE)/
                                     mean(h2o_delta, na.rm = TRUE),
                  C_emp_slope = coef(lm(co2_delta_q ~ co2_delta - 1))[1],
                  H2O_emp_slope = coef(lm(h2o_delta_q ~ h2o_delta - 1))[1]
                  ),
              by = .(full_scale_value, qthreshold, 
                     directional_diffusion, station)]

# ------------------------------------------------------------------------------

if (save_output) {
    message("Saving summarized simultion results")
    saveRDS(sim_errors, sim_errors_fn)
    saveRDS(nzero_w_err_dt, nzero_w_err_fn)
}

