# Read and summarize results

source("src/simulation/deps.R")
source("src/config.R")

# Read simulation reults -------------------------------------------------------

message("Reading results files")

sim_param_spc <- readRDS(sim_param_spc_fn)
sim_perf_eval  <- readRDS(sim_perf_eval_fn)


# ------------------------------------------------------------------------------
# Error summary
message("Summarizing errors")
sim_errors <- 
    rbind(sim_param_spc[, c(error_summary(fe_co2_q, co2_flux), 
                            list(Er_norm = sd(fe_co2_norm)),
                            scalar = "co2"), 
              by = .(full_scale_value, qthreshold, directional_diffusion, station)],
          sim_param_spc[, c(error_summary(fe_h2o_q, h2o_flux), 
                            list(Er_norm = sd(fe_h2o_norm)),
                            scalar = "h2o"), 
              by = .(full_scale_value, qthreshold, directional_diffusion, station)],
          sim_param_spc[, c(error_summary(fe_ts_q, ts_flux), 
                            list(Er_norm = sd(fe_Ts_norm)),
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
# Paramter space simulation

# Build results table
# Error definition, verify
sim_perf_eval[, all.equal(co2_flux - fe_co2_bts, co2_flux_bts) ]

results_tbl <- 
    rbind(
      sim_perf_eval[, c(error_summary(fe_co2_q, co2_flux), 
                 method = "QEA", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_co2_q0, co2_flux), 
                 method = "QEA0", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_co2_bts, co2_flux), 
                 method = "REA-BTS", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_co2_ldb_bts, co2_flux), 
                 method = "REA-LDB-BTS", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_co2_ldb_bcst, co2_flux), 
                 method = "REA-LDB-BCST", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_co2_ldb_bw, co2_flux), 
                 method = "REA-LDB-BW", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_q, h2o_flux), 
                 method = "QEA", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_q0, h2o_flux), 
                 method = "QEA0", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_bts, h2o_flux), 
                 method = "REA-BTS", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_ldb_bts, h2o_flux), 
                 method = "REA-LDB-BTS", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_ldb_bcst, h2o_flux), 
                 method = "REA-LDB-BCST", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_ldb_bw, h2o_flux), 
                 method = "REA-LDB-BW", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_ts_q, ts_flux), 
                 method = "QEA", scalar = "ts"), by = station],
      sim_perf_eval[, c(error_summary(fe_ts_ldb_bw, ts_flux), 
                 method = "REA-LDB-BW", scalar = "ts"), by = station],
          # DB1 treatments
      sim_perf_eval[, c(error_summary(fe_co2_db1_bts, co2_flux), 
                 method = "REA-DB1-BTS", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_co2_db1_bcst, co2_flux), 
                 method = "REA-DB1-BCST", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_co2_db1_bw, co2_flux), 
                 method = "REA-DB1-BW", scalar = "co2"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_db1_bts, h2o_flux), 
                 method = "REA-DB1-BTS", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_db1_bcst, h2o_flux), 
                 method = "REA-DB1-BCST", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_h2o_db1_bw, h2o_flux), 
                 method = "REA-DB1-BW", scalar = "h2o"), by = station],
      sim_perf_eval[, c(error_summary(fe_ts_db1_bw, ts_flux), 
                 method = "REA-DB1-BW", scalar = "ts"), by = station]
      )

# ------------------------------------------------------------------------------
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
                  open_rate_hz = mean(N_q/1800, na.rm = TRUE),
                  open_rate_hz_sd = sd(N_q/1800, na.rm = TRUE),
                  open_rate_up_hz = mean(N_up_q/1800, na.rm = TRUE),
                  open_rate_down_hz = mean(N_down_q/1800, na.rm = TRUE),
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

results_tbl_t_co2 <- 
    transpose(results_tbl[scalar == "co2" & station == "BS", !c("scalar", "station")], 
              make.names = "method", keep.names = "Metric")

results_tbl_t_h2o <- 
    transpose(results_tbl[scalar == "h2o" & station == "BS", !c("scalar", "station")], 
              make.names = "method", keep.names = "Metric")

results_tbl_t_ts <- 
    transpose(results_tbl[scalar == "ts" & station == "BS", !c("scalar", "station")], 
              make.names = "method", keep.names = "Metric")

# Improvements as ratios across all metrics
# -----------------------------------------

cols <- c("QEA0", "REA-BTS", "REA-LDB-BTS", "REA-LDB-BCST", "REA-LDB-BW", 
          "REA-DB1-BTS", "REA-DB1-BCST", "REA-DB1-BW")

results_tbl_t_co2[, (paste0(cols, "_rel")) := 
                  lapply(.SD, function(x) round(abs(x/QEA), 2)), .SDcols = cols]

setcolorder(results_tbl_t_co2, sort(names(results_tbl_t_co2)))

# Improvements only
results_tbl_t_co2[, .SD, .SDcols = patterns("_rel")]

# Add new columns
results_tbl_t_h2o[, (paste0(cols, "_rel")) := 
                  lapply(.SD, function(x) round(abs(x/QEA), 2)), .SDcols = cols]

# Only relative improvements
results_tbl_t_h2o[, .SD, .SDcols = patterns("(^QEA$|_rel$)"), by = Metric]

results_tbl_t_ts[, (paste0(cols, "_rel")) := 
                  lapply(.SD, function(x) round(abs(x/QEA), 2)), 
              by = .(Metric, QEA)]

# For measuring average improvment across different metrics
#     we consider the average of these variants as the baseline
baseline_methods_ref <- c("REA-LDB-BCST_rel", "REA-LDB-BW_rel", "REA-DB1-BW_rel")
baseline_methods_ref_ts <- c("REA-LDB-BW_rel", "REA-DB1-BW_rel")

results_tbl_t_co2[, avg_improvement := rowMeans(.SD), .SDcols = baseline_methods_ref]
results_tbl_t_h2o[, avg_improvement := rowMeans(.SD), .SDcols = baseline_methods_ref]
results_tbl_t_ts[, avg_improvement := rowMeans(.SD), .SDcols = baseline_methods_ref_ts]

results_tbl_t <- 
    rbind(results_tbl_t_h2o[, scalar := "h2o"], 
          results_tbl_t_co2[, scalar := "co2"], 
          results_tbl_t_ts[, scalar := "ts"], fill = TRUE)

results_tbl_summary <- 
    dcast(
          results_tbl_t[Metric %in% c("Er_mean_rel", "MAE_rel", 
                                    "Er_slope", "Er_slope1", 
                                    "Er_rmse"), 
                      .(Metric, avg_improvement), by = scalar], 
          Metric ~ scalar, value.var = "avg_improvement")

results_tbl_summary[, mean_improvement := rowMeans(.SD), by = Metric]

# ------------------------------------------------------------------------------

metrics_for_paper <- c( "scalar", "method", "Er_slope", "Er_intercept", 
                       "Er_mean_rel", "Er_rmse")

methods_for_paper <- c("QEA", "REA-LDB-BCST", "REA-LDB-BW", "REA-DB1-BW")

results_xtable <- 
    results_tbl[station == "BS" & scalar %in% c("co2", "h2o", "ts") & method %in% methods_for_paper,
                .SD, .SD = metrics_for_paper]

results_xtable <-  results_xtable[order(scalar)]


# xtable(results_xtable, digits = 6)

if (save_output) {
    message("Saving summarized simultion results")
    saveRDS(results_tbl, results_tbl_fn)
    saveRDS(sim_errors, sim_errors_fn)
    saveRDS(nzero_w_err_dt, nzero_w_err_fn)
    fwrite(results_tbl_t, results_tbl_t_fn)
    fwrite(results_xtable, results_xtable_fn)
    fwrite(results_tbl_summary, results_tbl_summary_fn)
}

