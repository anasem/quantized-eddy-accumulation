# This file is typicall run after loading the simulation data and the
# configuration, see run-best-case-senario.R

# Compare the two methods under best case senarios for both of them

# Treatments:
# * _ldb linear deadband 0.5 w_sd (Vogl et al)
# * _db1 linear deadband 0.9 w_sd (Baker)
# * fe_  flux error defined as fe = true - measured 

# co2_flux_bts      REA CO2 flux with beta calcualted from Ts
# co2_flux_ban      REA CO2 flux calcualted with analaytical beta
# co2_flux_bw       REA CO2 flux calcualted with beta from wind statistics
# co2_flux_bh2o     REA CO2 flux with beta calcualted from H2O
# co2_flux_ldb_bts  REA CO2 flux with beta calcualted from H2O
# co2_flux_ldb_bh2o REA CO2 flux with beta calcualted from H2O with 0.5w_sd
#                   linear deadband 
# co2_flux_ldb_bw   REA CO2 flux with beta calcualted from wind stats with 0.5w_sd
#                   linear deadband 
# co2_flux_ldb_bcst REA CO2 flux with linear deadband and beta as the median of
#                   Beta calcualted from sonic temperature
# co2_flux_db1_bts  REA flux from beta Ts and deadband of 0.9w_sd
# co2_flux_db1_bh2o REA flux from beta H2O and deadband of 0.9w_sd
# co2_flux_db1_bw   REA flux from beta wind stats and deadband of 0.9w_sd
# co2_flux_db1_bcst REA flux from beta clacualted from the median of Beta_ts and
#                   and a deadband of 0.9w_sd


# Baseline REA treatments ------------------------------------------------------
# See alo Vogl et al. 2021
# Define rollback windows to estimate the dynamic deadband
# It guarantees temporal consistency (resistant to jumps)
ec_dt[, window_id := floor(as.numeric(time)/lookback_window)]

# Rollback window threshold, preivous window
# This will be merged later, so each sd is calculated from the previous
# window. I.e the current window stats will be used in window_id + 1
rb_window_thresholds <- 
    ec_dt[, .(window_id = first(window_id) +1,
              deadband_threshold = sd(w, na.rm = TRUE)*dynamic_deadband_width), 
         by = .(original_window_id = window_id)]

# Merge rollback window thresolds with ec
ec_dt <- merge(ec_dt, 
               rb_window_thresholds[, !"original_window_id"],
               by ="window_id", all.x = TRUE)

# Execlude jumps at starts (previous window is NA)
# This modifying ec_dt, making it incomparable to the param sim simulation
ec_dt <- ec_dt[!is.na(deadband_threshold)]

# Define deadband flag, when |w|< threshold
ec_dt[, is_deadband :=  abs(w) < deadband_threshold]

# Ratio of values flagged as deadband in the whole dataset
ec_dt[, sum(is_deadband, na.rm = TRUE)/.N]

# Baseline treatments ----------------------------------------------------------
# Dynamic linear band with lookback window
message("Calculating baseline treatments")
sim_dt_ldb <- 
    ec_dt[is_deadband == FALSE, 
          .(co2_flux_ldb = bcov(w, CO2),
            co2_up_mean_ldb = mean(CO2[is_updraft], na.rm = TRUE),
            co2_down_mean_ldb = mean(CO2[!is_updraft], na.rm = TRUE),
            h2o_up_mean_ldb = mean(H2O[is_updraft], na.rm = TRUE),
            h2o_down_mean_ldb = mean(H2O[!is_updraft], na.rm = TRUE),
            w_up_mean_ldb  = mean(w[is_updraft], na.rm =TRUE),
            w_down_mean_ldb  = mean(w[!is_updraft], na.rm =TRUE),
            N_up_ldb = sum(is_updraft, na.rm = TRUE),
            N_down_ldb = sum(!is_updraft, na.rm = TRUE),
            N_tot_ldb = .N,
            ts_up_mean_ldb = mean(Ts[is_updraft], na.rm = TRUE),
            ts_down_mean_ldb = mean(Ts[!is_updraft], na.rm = TRUE),
            w_sd_ldb = sd(w, na.rm = TRUE), 
            r_wc_ldb = cor(w, CO2),
            r_wTs_ldb = cor(w, Ts),
            co2_sd_ldb = sd(CO2, na.rm = TRUE),
            co2_mean_ldb = mean(CO2, na.rm = TRUE),
            Ts_sd_ldb = sd(Ts, na.rm = TRUE),
            w_mean_ldb = mean(w, na.rm = TRUE)),
    by = aiid]

sim_dt_ldb[, co2_delta_ldb := co2_up_mean_ldb - co2_down_mean_ldb]
sim_dt_ldb[, ts_delta_ldb := ts_up_mean_ldb - ts_down_mean_ldb]
sim_dt_ldb[, h2o_delta_ldb := h2o_up_mean_ldb - h2o_down_mean_ldb]
sim_dt_ldb[, beta_an_ldb := beta_analytic(w_mean_ldb, w_sd_ldb)]
sim_dt_ldb[, w_delta_ldb := w_up_mean_ldb - w_down_mean_ldb ]

# Deadband with 0.9 sigma w;
# Should be from previous aiid but here we just use the same one
ec_dt[, w_sd_ai := sd(w), by = aiid]

sim_dt_db1 <- 
    ec_dt[abs(w) > 0.9*w_sd_ai, 
          .(co2_flux_db1 = bcov(w, CO2),
            co2_up_mean_db1 = mean(CO2[is_updraft], na.rm = TRUE),
            co2_down_mean_db1 = mean(CO2[!is_updraft], na.rm = TRUE),
            h2o_up_mean_db1 = mean(H2O[is_updraft], na.rm = TRUE),
            h2o_down_mean_db1 = mean(H2O[!is_updraft], na.rm = TRUE),
            w_up_mean_db1  = mean(w[is_updraft], na.rm =TRUE),
            w_down_mean_db1  = mean(w[!is_updraft], na.rm =TRUE),
            N_up_db1 = sum(is_updraft, na.rm = TRUE),
            N_down_db1 = sum(!is_updraft, na.rm = TRUE),
            N_tot_db1 = .N,
            ts_up_mean_db1 = mean(Ts[is_updraft], na.rm = TRUE),
            ts_down_mean_db1 = mean(Ts[!is_updraft], na.rm = TRUE),
            w_sd_db1 = sd(w, na.rm = TRUE), 
            r_wc_db1 = cor(w, CO2),
            r_wTs_db1 = cor(w, Ts),
            co2_sd_db1 = sd(CO2, na.rm = TRUE),
            co2_mean_db1 = mean(CO2, na.rm = TRUE),
            Ts_sd_db1 = sd(Ts, na.rm = TRUE),
            w_mean_db1 = mean(w, na.rm = TRUE)),
    by = aiid]


sim_dt_db1[, co2_delta_db1 := co2_up_mean_db1 - co2_down_mean_db1]
sim_dt_db1[, ts_delta_db1 := ts_up_mean_db1 - ts_down_mean_db1]
sim_dt_db1[, h2o_delta_db1 := h2o_up_mean_db1 - h2o_down_mean_db1]
sim_dt_db1[, beta_an_db1 := beta_analytic(w_mean_db1, w_sd_db1)]
sim_dt_db1[, w_delta_db1 := w_up_mean_db1 - w_down_mean_db1 ]

sim_dt_summary <- 
    ec_dt[, .(time = first(time),
              co2_flux = bcov(w, CO2),
              ts_flux = bcov(w, Ts),
              h2o_flux = bcov(w, H2O),
              co2_up_mean = mean(CO2[is_updraft], na.rm = TRUE),
              co2_down_mean = mean(CO2[!is_updraft], na.rm = TRUE),
              h2o_up_mean  = mean(H2O[is_updraft], na.rm = TRUE),
              h2o_down_mean = mean(H2O[!is_updraft], na.rm = TRUE),
              h2o_mean = mean(H2O, na.rm = TRUE),
              N_up = sum(is_updraft, na.rm = TRUE),
              N_down = sum(!is_updraft, na.rm = TRUE),
              N_tot = .N,
              Ts_up_mean = mean(Ts[is_updraft], na.rm = TRUE),
              Ts_down_mean = mean(Ts[!is_updraft], na.rm = TRUE),
              Ts_mean = mean(Ts, na.rm = TRUE),
              w_sd = sd(w, na.rm = TRUE), 
              w_max = max(w, na.rm = TRUE),
              r_wc = cor(w, CO2),
              r_wTs = cor(w, Ts),
              r_wH2o = cor(w, H2O),
              co2_sd = sd(CO2, na.rm = TRUE),
              h2o_sd = sd(H2O, na.rm = TRUE),
              co2_mean = mean(CO2, na.rm = TRUE),
              Ts_sd = sd(Ts, na.rm = TRUE),
              w_mean = mean(w, na.rm = TRUE),
              w_abs_mean = mean(abs(w), na.rm = TRUE),
              zL = z_height/ Obukhov_length_uvw(u,v,w,Ts, P = atm_pressure),
              ustar = friction_velocity_uvw(u,v,w),
              U = mean(wind_speed(u, v)),
              Rs_wTs = steady_state_ratio(w, Ts),
              Rs_wH2o = steady_state_ratio(w, H2O),
              Rs_wc = steady_state_ratio(w, CO2),
              beta_co2 = rea_beta(w, CO2),
              beta_ts = rea_beta(w, Ts),
              beta_h2o = rea_beta(w, H2O),
              beta_w = rea_beta_w(w)),
    by = aiid]


# Convert h2o form mmol/m3 to kg/m3
H2O_molar_mass <- 18.01528e-3
sim_dt_summary[, rho_v := h2o_mean * H2O_molar_mass / 1000]
sim_dt_summary[, ew := water_vapor_partial_pressure(rho_v, Ts_mean)]
sim_dt_summary[, rho_a := dry_air_density(P_a = 1013.25 * 100 - ew, Ta  = Ts_mean)]
sim_dt_summary[, rho := rho_a + rho_v]
# Specific humidity
sim_dt_summary[, Q := rho_v/rho]
# Specific heat capacity for dry air
sim_dt_summary[, Cp_a := gas_heat_capacity(Ts_mean, "air")]
# Specific  heat capacity for water vapor
sim_dt_summary[, Cp_v := gas_heat_capacity(Ts_mean, "H2O")]
sim_dt_summary[, Cp := Cp_a *(1-Q)+ Cp_v*Q]


sim_dt_summary[, co2_delta := (co2_up_mean - co2_down_mean)]
sim_dt_summary[, h2o_delta := (h2o_up_mean - h2o_down_mean)]
sim_dt_summary[, beta_an := beta_analytic(w_mean, w_sd)]
sim_dt_summary[, beta_const := median(beta_ts, na.rm = TRUE)]
# Validate REA fluxes
sim_dt_summary[, all.equal(co2_flux, co2_delta*beta_co2*w_sd)]

sim_base1 <- merge(sim_dt_ldb, sim_dt_summary, by = "aiid")
sim_base_treatments <- merge(sim_base1, sim_dt_db1, by = "aiid")

sim_base_treatments[, all.equal(N_up_ldb + N_down_ldb, N_tot_ldb)]
sim_base_treatments[, all.equal(N_up_db1 + N_down_db1, N_tot_db1)]

# Calcualte beta's
sim_base_treatments[, beta_co2_ldb   := rea_beta2(co2_flux, w_sd, co2_delta_ldb)]
sim_base_treatments[, beta_ts_ldb    := rea_beta2(ts_flux, w_sd, ts_delta_ldb)]
sim_base_treatments[, beta_h2o_ldb   := rea_beta2(h2o_flux, w_sd, h2o_delta_ldb)]
sim_base_treatments[, beta_w_ldb     := rea_beta_w2(w_delta_ldb, w_sd)]
sim_base_treatments[, beta_co2_db1   := rea_beta2(co2_flux, w_sd, co2_delta_db1)]
sim_base_treatments[, beta_ts_db1    := rea_beta2(ts_flux, w_sd, ts_delta_db1)]
sim_base_treatments[, beta_h2o_db1   := rea_beta2(h2o_flux, w_sd, h2o_delta_db1)]
sim_base_treatments[, beta_w_db1     := rea_beta_w2(w_delta_db1, w_sd)]
sim_base_treatments[, beta_ts_const_ldb := median(beta_ts_ldb, na.rm = TRUE)]
sim_base_treatments[, beta_ts_const_db1 := median(beta_ts_db1, na.rm = TRUE)]

# Calculate REA fluxes for CO2
sim_base_treatments[, co2_flux_bts      := co2_delta * beta_ts * w_sd]
sim_base_treatments[, co2_flux_ban      := co2_delta * beta_an * w_sd]
sim_base_treatments[, co2_flux_bw       := co2_delta * beta_w * w_sd]
sim_base_treatments[, co2_flux_bh2o     := co2_delta * beta_h2o * w_sd]
sim_base_treatments[, co2_flux_ldb_bts  := co2_delta_ldb * beta_ts_ldb * w_sd]
sim_base_treatments[, co2_flux_ldb_bh2o := co2_delta_ldb * beta_h2o_ldb * w_sd]
sim_base_treatments[, co2_flux_ldb_bw   := co2_delta_ldb * beta_w_ldb * w_sd]
sim_base_treatments[, co2_flux_ldb_bcst := co2_delta_ldb * beta_ts_const_ldb * w_sd]

sim_base_treatments[, co2_flux_db1_bts  := co2_delta_db1 * beta_ts_db1 * w_sd]
sim_base_treatments[, co2_flux_db1_bh2o := co2_delta_db1 * beta_h2o_db1 * w_sd]
sim_base_treatments[, co2_flux_db1_bw   := co2_delta_db1 * beta_w_db1 * w_sd]
sim_base_treatments[, co2_flux_db1_bcst := co2_delta_db1 * beta_ts_const_db1 * w_sd]

sim_base_treatments[, h2o_flux_bts     := h2o_delta * beta_ts * w_sd]
sim_base_treatments[, h2o_flux_ban     := h2o_delta * beta_an * w_sd]
sim_base_treatments[, h2o_flux_bco2    := h2o_delta * beta_co2 * w_sd]
sim_base_treatments[, h2o_flux_bw      := h2o_delta * beta_w * w_sd]
sim_base_treatments[, h2o_flux_ldb_bts := h2o_delta_ldb * beta_ts_ldb * w_sd]
sim_base_treatments[, h2o_flux_ldb_co2 := h2o_delta_ldb * beta_co2_ldb * w_sd]
sim_base_treatments[, h2o_flux_ldb_bw  := h2o_delta_ldb * beta_w_ldb * w_sd]
sim_base_treatments[, h2o_flux_ldb_bcst := h2o_delta_ldb * beta_ts_const_ldb * w_sd]

sim_base_treatments[, h2o_flux_db1_bts := h2o_delta_db1 * beta_ts_db1 * w_sd]
sim_base_treatments[, h2o_flux_db1_co2 := h2o_delta_db1 * beta_co2_db1 * w_sd]
sim_base_treatments[, h2o_flux_db1_bw  := h2o_delta_db1 * beta_w_db1 * w_sd]
sim_base_treatments[, h2o_flux_db1_bcst := h2o_delta_db1 * beta_ts_const_db1 * w_sd]

sim_base_treatments[, ts_flux_ldb_bw   := ts_delta_ldb * beta_w_ldb * w_sd]
sim_base_treatments[, ts_flux_db1_bw   := ts_delta_db1 * beta_w_db1 * w_sd]


# ------------------------------------------------------------------------------
# QEA treatments
# Under optimal parameters
message("Calculating QEA treatments")

# Quantization with error diffusion
ec_dt[, wq := quantize_w(w, sd(w)*quantization_threshold, 
                         full_scale_value = sd(w)*full_scale_value, 
                         directional_diffusion = directional_diffusion, 
                         damping_factor = 1), 
      by = aiid]

# Quantization without error diffusion
ec_dt[, wq0 := quantize_w(w, sd(w)*q0_quantization_threshold, 
                          full_scale_value = 1, 
                          directional_diffusion = FALSE, damping_factor = 0), 
       by = aiid]

# Modified input
ec_dt[, w_adj := quantize_w(w, sd(w)*quantization_threshold, 
                            full_scale_value = sd(w)*full_scale_value, 
                            return_modified = TRUE, 
                            directional_diffusion = directional_diffusion, 
                            damping_factor = 1), by = aiid]
# Quantizer error
ec_dt[, er_q := wq - w_adj]
# Diffusion error
ec_dt[, er_d := w - w_adj]

ec_dt[, is_updraft_q := NA]
ec_dt[wq > 0, is_updraft_q := TRUE]
ec_dt[wq < 0, is_updraft_q := FALSE]

sim_qea <- ec_dt[, .(alpha_co2_q = alpha_xy(CO2, wq),
                      alpha_ts_q = alpha_xy(Ts, wq),
                      alpha_h2o_q = alpha_xy(H2O, wq),
                      co2_up_mean_q = mean(CO2[is_updraft_q], na.rm = TRUE),
                      co2_down_mean_q = mean(CO2[!is_updraft_q], na.rm = TRUE),
                      co2_mean_q = mean(CO2[wq !=0]),
                      h2o_up_mean_q = mean(H2O[is_updraft_q], na.rm = TRUE),
                      h2o_down_mean_q = mean(H2O[!is_updraft_q], na.rm = TRUE),
                      ts_up_mean_q = mean(Ts[is_updraft_q], na.rm = TRUE),
                      ts_down_mean_q = mean(Ts[!is_updraft_q], na.rm = TRUE),
                      wq_mean = mean(wq, na.rm = TRUE),
                      N_up_q = sum(is_updraft_q, na.rm = TRUE),
                      N_down_q = sum(!is_updraft_q, na.rm = TRUE),
                      N_q = sum(wq != 0), 
                      wq0_sd = sd(wq0, na.rm = TRUE),
                      wq_sd = sd(wq, na.rm = TRUE),
                      wqr_sd = sd(w - wq, na.rm = TRUE),
                      er_q_sd = sd(er_q, na.rm = TRUE),
                      er_q_w_r = cor(er_q, w),
                      er_q_co2_r = cor(er_q, CO2),
                      er_d_sd = sd(er_d, na.rm = TRUE),
                      er_d_co2_r = cor(er_d, CO2),
                      co2_flux_lag1 = bcov(CO2, shift(w, 1)),
                      wq_abs_mean = mean(abs(wq)),
                      co2_flux_q = bcov(CO2, wq),
                      ts_flux_q = bcov(Ts, wq),
                      h2o_flux_q = bcov(H2O, wq),
                      h2o_flux_q0 = bcov(H2O, wq0),
                      co2_flux_q0 = bcov(CO2, wq0)),
                   by = aiid]

message("Merging results")
perf_eval <- merge(sim_base_treatments, sim_qea, by = "aiid")

# Summarize results  -----------------------------------------------------------

perf_eval[, station := station_name]

# Define flux errors as true - measured; preifix fe_: flux error
perf_eval[, co2_flux_q0_cc  := co2_flux_q0 * w_sd/ wq0_sd]
perf_eval[, h2o_flux_q0_cc  := h2o_flux_q0 * w_sd/ wq0_sd]
perf_eval[, fe_co2_bts      := co2_flux - co2_flux_bts ]
perf_eval[, fe_h2o_bts      := h2o_flux - h2o_flux_bts]
perf_eval[, fe_co2_ldb_bts  := co2_flux - co2_flux_ldb_bts]
perf_eval[, fe_h2o_ldb_bts  := h2o_flux - h2o_flux_ldb_bts]
perf_eval[, fe_co2_ldb_bcst := co2_flux - co2_flux_ldb_bcst]
perf_eval[, fe_h2o_ldb_bcst := h2o_flux - h2o_flux_ldb_bcst]
perf_eval[, fe_co2_ldb_bw   := co2_flux - co2_flux_ldb_bw]
perf_eval[, fe_h2o_ldb_bw   := h2o_flux - h2o_flux_ldb_bw]
perf_eval[, fe_ts_ldb_bw    := ts_flux - ts_flux_ldb_bw ]
#
perf_eval[, fe_co2_q        := co2_flux - co2_flux_q]
perf_eval[, fe_h2o_q        := h2o_flux - h2o_flux_q]
perf_eval[, fe_ts_q         := ts_flux - ts_flux_q]
perf_eval[, fe_co2_q0       := co2_flux - co2_flux_q0_cc]
perf_eval[, fe_h2o_q0       := h2o_flux - h2o_flux_q0_cc]
#
# Treatments with DB1
perf_eval[, fe_co2_db1_bw   := co2_flux - co2_flux_db1_bw]
perf_eval[, fe_h2o_db1_bw   := h2o_flux - h2o_flux_db1_bw]
perf_eval[, fe_ts_db1_bw    := ts_flux - ts_flux_db1_bw ]
#
perf_eval[, fe_co2_db1_bts  := co2_flux - co2_flux_db1_bts]
perf_eval[, fe_h2o_db1_bts  := h2o_flux - h2o_flux_db1_bts]
perf_eval[, fe_co2_db1_bcst := co2_flux - co2_flux_db1_bcst]
perf_eval[, fe_h2o_db1_bcst := h2o_flux - h2o_flux_db1_bcst]

# Non dimentional errors defined as flux_error/ w_sd / c_sd
perf_eval[, ndfe_co2_q  := fe_co2_q/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_q  := fe_h2o_q/ w_sd / h2o_sd]
perf_eval[, ndfe_Ts_q   := fe_ts_q/w_sd/Ts_sd  ]
perf_eval[, ndfe_theory := 1/sqrt(N_tot) * wqr_sd / w_sd ]

perf_eval[, ndfe_co2_q0       := fe_co2_q0/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_q0       := fe_h2o_q0/ w_sd / h2o_sd]
perf_eval[, ndfe_co2_bts      := fe_co2_bts/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_bts      := fe_h2o_bts/ w_sd / h2o_sd]
perf_eval[, ndfe_co2_ldb_bts  := fe_co2_ldb_bts/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_ldb_bts  := fe_h2o_ldb_bts/ w_sd / h2o_sd]
perf_eval[, ndfe_co2_ldb_bcst := fe_co2_ldb_bcst/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_ldb_bcst := fe_h2o_ldb_bcst/ w_sd / h2o_sd]
perf_eval[, ndfe_co2_ldb_bw   := fe_co2_ldb_bw/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_ldb_bw   := fe_h2o_ldb_bw/ w_sd / h2o_sd]
perf_eval[, ndfe_ts_ldb_bw    := fe_ts_ldb_bw/ w_sd/ Ts_sd]
perf_eval[, ndfe_co2_db1_bw   := fe_co2_db1_bw/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_db1_bw   := fe_h2o_db1_bw/ w_sd / h2o_sd]
perf_eval[, ndfe_ts_db1_bw    := fe_ts_db1_bw/ w_sd/ Ts_sd]
perf_eval[, ndfe_co2_db1_bts  := fe_co2_db1_bts/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_db1_bts  := fe_h2o_db1_bts/ w_sd / h2o_sd]
perf_eval[, ndfe_co2_db1_bcst := fe_co2_db1_bcst/ w_sd/ co2_sd]
perf_eval[, ndfe_h2o_db1_bcst := fe_h2o_db1_bcst/ w_sd / h2o_sd]


perf_eval[, zl_cat := cut_zL3(zL)]
perf_eval <- perf_eval[order(zl_cat)]
perf_eval[, QC := "Low"]
perf_eval[Rs_wc < qc_Rs_wc_smaller_than & ustar > qc_ustar_greater_than, QC := "OK"]

# Save output ------------------------------------------------------------------

if (save_output) {
    message("Saving output")
    saveRDS(perf_eval, file = sim_perf_eval_fn)
}


