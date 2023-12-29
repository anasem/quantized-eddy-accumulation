source("src/simulation/deps.R")
source("src/config.R")
source("src/simulation/load-simulation-data.R")

message("Running parameter space simulation")

# ------------------------------------------------------------------------------
message("Calculating summary statistics")

sim_dt_summary <- 
    ec_dt[, .(time = first(time),
              alpha_co2 = alpha_xy(CO2, w),
              alpha_ts = alpha_xy(Ts, w),
              alpha_h2o = alpha_xy(H2O, w),
              co2_flux = bcov(w, CO2),
              ts_flux = bcov(w, Ts),
              h2o_flux = bcov(w, H2O),
              co2_up_mean = mean(CO2[is_updraft], na.rm = TRUE),
              co2_down_mean = mean(CO2[!is_updraft], na.rm = TRUE),
              h2o_up_mean  = mean(H2O[is_updraft], na.rm = TRUE),
              h2o_down_mean = mean(H2O[!is_updraft], na.rm = TRUE),
              N_up = sum(is_updraft, na.rm = TRUE),
              N_down = sum(!is_updraft, na.rm = TRUE),
              N_tot = .N,
              Ts_up_mean = mean(Ts[is_updraft], na.rm = TRUE),
              Ts_down_mean = mean(Ts[!is_updraft], na.rm = TRUE),
              w_sd = sd(w, na.rm = TRUE), 
              w_max = max(w, na.rm = TRUE),
              r_wc = cor(w, CO2),
              r_wTs = cor(w, Ts),
              r_wH2o = cor(w, H2O),
              co2_sd = sd(CO2, na.rm = TRUE),
              h2o_sd = sd(H2O, na.rm = TRUE),
              Ts_sd = sd(Ts, na.rm = TRUE),
              co2_mean = mean(CO2, na.rm = TRUE),
              w_mean = mean(w, na.rm = TRUE),
              w_abs_mean = mean(abs(w), na.rm = TRUE),
              zL = z_height/ Obukhov_length_uvw(u,v,w,Ts, P = atm_pressure),
              ustar = friction_velocity_uvw(u,v,w),
              U = mean(wind_speed(u, v)),
              beta_co2 = rea_beta(w, CO2),
              beta_ts = rea_beta(w, Ts),
              beta_h2o = rea_beta(w, H2O),
              beta_w = rea_beta_w(w)),
    by = aiid]

sim_dt_summary[, co2_delta := (co2_up_mean - co2_down_mean)]
sim_dt_summary[, h2o_delta := (h2o_up_mean - h2o_down_mean)]
sim_dt_summary[, beta_an := beta_analytic(w_mean, w_sd)]
sim_dt_summary[, beta_const := median(beta_ts, na.rm = TRUE)]

# Validate REA fluxes
# sim_dt_summary[, all.equal(co2_flux, co2_delta*beta_co2*w_sd)]


# Generate combinations --------------------------------------------------------


combinations <- 
    data.table(expand.grid(qthreshold = qthreshold_vals,
                           full_scale_value = full_scale_vals,
                           directional_diffusion = c(TRUE, FALSE)))

combinations <- 
    combinations[qthreshold < full_scale_value]

combinations[, treatment_id := seq_along(qthreshold)]


# ------------------------------------------------------------------------------

process_aiid_group <- function(ec_dt_group, combinations) {
    dt_list <- vector("list", nrow(combinations))
    message("Started processing group")
    devider <- sample(0:9, 1)
    t0 <- Sys.time()
    for (row_index in 1:nrow(combinations)) {
        sim_dt_q <- simulate_combination(ec_dt_group, combinations[row_index, ])
        dt_list[[row_index]] <- cbind(copy(sim_dt_q), combinations[row_index, ])

        # Just a simple way to track progress
        if (row_index %% 10 == devider) {
            t1 <- Sys.time()
            time_estimates <- as_hms(floor((t1 - t0)/10 *
                                    (nrow(combinations) - row_index)))
            if (row_index > 10) {
                message("Finsihed treatment: ", row_index, "/", nrow(combinations),
                        ".. est. remaining: ", time_estimates)
            }
            t0 <- Sys.time()
        }

    }
    message("Finished processing group")
    return(rbindlist(dt_list))
}


simulate_combination <- function(ec_dt, combination) {
    qthreshold_            <- combination$qthreshold
    high_level_            <- combination$full_scale_value
    directional_diffusion_ <- combination$directional_diffusion

    ec_dt[, wq := quantize_w(w, sd(w)*qthreshold_, 
                             full_scale_value = sd(w)*high_level_, 
                             directional_diffusion = directional_diffusion_, 
                             damping_factor = 1), 
          by = aiid]

    # Quantization without error diffusion
    ec_dt[, wq0 := quantize_w(w, sd(w)*qthreshold_, 
                              full_scale_value = sd(w)*high_level_, 
                              directional_diffusion = TRUE, 
                              damping_factor = 0), 
          by = aiid]

    # Modified input
    ec_dt[, w_adj := quantize_w(w, sd(w)*qthreshold_, 
                                full_scale_value = sd(w)*high_level_, 
                                return_modified = TRUE, 
                                directional_diffusion = directional_diffusion_, 
                                damping_factor = 1), 
          by = aiid]

    # Quantizer error
    ec_dt[, er_q := wq - w_adj]

    # Diffusion error
    ec_dt[, er_d := w - w_adj]

    # Quantization error
    ec_dt[, wqr := w - wq]
    ec_dt[, all.equal(w - wq,  er_d - er_q)]

    ec_dt[, is_updraft_q := NA]
    ec_dt[wq > 0, is_updraft_q := TRUE]
    ec_dt[wq < 0, is_updraft_q := FALSE]

    sim_dt_q <- ec_dt[, .(alpha_co2_q = alpha_xy(CO2, wq),
                          alpha_ts_q = alpha_xy(Ts, wq),
                          alpha_h2o_q = alpha_xy(H2O, wq),
                          co2_up_mean_q = mean(CO2[is_updraft_q], na.rm = TRUE),
                          co2_down_mean_q = mean(CO2[!is_updraft_q], na.rm = TRUE),
                          co2_mean_q = mean(CO2[wq !=0]),
                          co2_sd_q = sd(CO2[wq !=0]),
                          h2o_up_mean_q = mean(H2O[is_updraft_q], na.rm = TRUE),
                          h2o_down_mean_q = mean(H2O[!is_updraft_q], na.rm = TRUE),
                          ts_up_mean_q = mean(Ts[is_updraft_q], na.rm = TRUE),
                          ts_down_mean_q = mean(Ts[!is_updraft_q], na.rm = TRUE),
                          wq_mean = mean(wq, na.rm = TRUE),
                          wq_mean_up = mean(wq[is_updraft_q], na.rm = TRUE),
                          wq_mean_down = mean(wq[!is_updraft_q], na.rm = TRUE),
                          N_up_q = sum(is_updraft_q, na.rm = TRUE),
                          N_down_q = sum(!is_updraft_q, na.rm = TRUE),
                          N_q = sum(wq != 0), 
                          wq0_sd = sd(wq0, na.rm = TRUE),
                          wq_sd = sd(wq, na.rm = TRUE),
                          er_q_sd = sd(er_q, na.rm = TRUE),
                          er_q_mean = mean(er_q, na.rm = TRUE),
                          er_q_w_r = cor(er_q, w),
                          er_q_co2_r = cor(er_q, CO2),
                          er_d_sd = sd(er_d, na.rm = TRUE),
                          er_d_mean = mean(er_d, na.rm = TRUE),
                          wq_abs_mean = mean(abs(wq), na.rm = TRUE),
                          wqr_sd = sd(w - wq, na.rm = TRUE),
                          wq_rmse = sqrt(mean((w - wq)^2)),
                          r_wqr.co2 = cor(w - wq, CO2), 
                          co2_flux_q = bcov(CO2, wq),
                          fe_co2_flux_q = bcov(CO2, w - wq),
                          erq_co2 = bcov(er_q, CO2),
                          erd_co2 = bcov(er_d, CO2),
                          r_erd_erq = cor(er_q, er_d),
                          ts_flux_q = bcov(Ts, wq),
                          h2o_flux_q = bcov(H2O, wq),
                          h2o_flux_q0 = bcov(H2O, wq0),
                          co2_flux_q0 = bcov(CO2, wq0)),
                    by = aiid]

    return(sim_dt_q)
}

# Run simulation ---------------------------------------------------------------

ec_dt_splits <- split(ec_dt, cut(ec_dt[, aiid], 
                                 breaks= number_of_cores, labels=FALSE))
rm(ec_dt)

results <- mclapply(ec_dt_splits, process_aiid_group, combinations, 
                    mc.cores = number_of_cores)

sim_param_spc <- merge(rbindlist(results), sim_dt_summary, by = "aiid", 
                       all.x = TRUE)

# Calculate errors ------------------------------------------------------------

sim_param_spc[, station := station_name]

# fe: flux error = true_flux - quantized_flux
sim_param_spc[, fe_co2_q := co2_flux - co2_flux_q]
sim_param_spc[, fe_h2o_q := h2o_flux - h2o_flux_q]
sim_param_spc[, fe_ts_q := ts_flux - ts_flux_q]
sim_param_spc[, co2_delta_q := co2_up_mean_q - co2_down_mean_q]
sim_param_spc[, co2_delta := co2_up_mean - co2_down_mean]
sim_param_spc[, h2o_delta_q := h2o_up_mean_q - h2o_down_mean_q]
sim_param_spc[, h2o_delta := h2o_up_mean - h2o_down_mean]
sim_param_spc[, co2_flux_q0_cc := co2_flux_q0 * w_sd/wq0_sd]
sim_param_spc[, fe_co2_q0 := co2_flux - co2_flux_q0_cc]


# Normalized errors
sim_param_spc[, fe_h2o_q := h2o_flux - h2o_flux_q ]
sim_param_spc[, fe_ts_q := ts_flux - ts_flux_q]
sim_param_spc[, fe_co2_norm := fe_co2_q/ w_sd/ co2_sd]
sim_param_spc[, fe_h2o_norm := fe_h2o_q/ (h2o_flux/r_wH2o)]
sim_param_spc[, fe_Ts_norm := fe_ts_q/w_sd/Ts_sd  ]
sim_param_spc[, fe_theory := 1/sqrt(N_tot) * wqr_sd / w_sd ] # 


# Save output ------------------------------------------------------------------

if (save_output) {
    message("Saving output")
    saveRDS(sim_param_spc, file = sim_param_spc_fn)
}


