source("src/plots/deps.R")
source("src/config/Hai_and_BS.R")

# Load simulation results
perf_dt         <- readRDS(sim_perf_eval_fn)
results_tbl     <- readRDS(perf_metrics_fn)


co2_method_name.x  <- -30
co2_method_name.y  <- 7
point_size         <- 1.1
diurnal_ln_col     <- mp(8, palette =  palette_AE4)
text_color         <- mp(8, palette =  palette_AE4)
mean_lnw           <- 0.8


perf_dt[, zl_cat := cut_zL3(zL)]
perf_dt <- perf_dt[co2_flux < 10]

co2_flux_labs <-  function() {
    labs(x = expression(paste("Ref ", CO[2], " flux (", mu, mol, ~m^-2,~s^-1, ")")),
         y = expression(paste("Flux error ", " (", mu, mol, ~m^-2,~s^-1, ")"))) 
}

h2o_flux_labs <-  function() {
    labs(x = expression(paste("Ref ", H[2], "O flux (", m, mol, ~m^-2,~s^-1, ")")),
         y = expression(paste("Flux error ", " (", m, mol, ~m^-2,~s^-1, ")"))) 
}



# ------------------------------------------------------------------------------

co2_ylim <- c(-12, 12)
station_name <- "Hainich"
perf_co2_bts     <- err_plot("REA-BTS", "co2", station_name,  ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_bcst    <- err_plot("REA-LDB-BCST", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_q       <- err_plot("QEA", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_ldb_bw  <- err_plot("REA-LDB-BW", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_db1_bw  <- err_plot("REA-DB1-BW", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_ldb_BTS <- err_plot("REA-LDB-BTS", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)



p_method_comparison_co2_Hai <- 
    (perf_co2_bcst + (perf_co2_ldb_bw + no_y_lab()) + (perf_co2_q + no_y_lab())) /
    (perf_co2_bts + (perf_co2_db1_bw + no_y_lab()) + (perf_co2_ldb_BTS + no_y_lab())) +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", 
          legend.box = "vertical", legend.box.just = "left") &
    guides(color = guide_legend(override.aes = list(size = 3)))


psave(p = p_method_comparison_co2_Hai, 
      name = "A1-comparison-against-REA-co2-Hai.pdf", 
      width = 14, height = 4.3*2.1, scale = 2,
      output_dir = plot_output_dir)

# ------------------------------------------------------------------------------

station_name <- "Hainich"
h2o_coord  <- function() coord_cartesian(xlim = c(-1.8, 6), ylim = c(-1.5,1.3))
err_plot_h2o  <- function(method) {
    err_plot(method, "h2o", station_name = "Hainich", 
             results_tbl = results_tbl,
             method_label_xy = c(0, 1), 
             error_label_xyy = c(-1.8, -1.3, -1.5)) + 
h2o_coord()
}

perf_h2o_bts     <- err_plot_h2o("REA-BTS") 
perf_h2o_bcst    <- err_plot_h2o("REA-LDB-BCST")
perf_h2o_q       <- err_plot_h2o("QEA")
perf_h2o_ldb_bw  <- err_plot_h2o("REA-LDB-BW" )
perf_h2o_db1_bw  <- err_plot_h2o("REA-DB1-BW")
perf_h2o_ldb_BTS    <- err_plot_h2o("REA-LDB-BTS")
#
p_method_comparison_h2o_Hai <- 
    (perf_h2o_bcst + (perf_h2o_ldb_bw + no_y_lab()) + (perf_h2o_q + no_y_lab())) /
    (perf_h2o_bts + (perf_h2o_db1_bw + no_y_lab()) + (perf_h2o_ldb_BTS + no_y_lab())) +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", 
          legend.box = "vertical", legend.box.just = "left") &
    guides(color = guide_legend(override.aes = list(size = 3))) &
    h2o_coord()


psave(p = p_method_comparison_h2o_Hai, 
      name = "A1-comparison-against-REA-h2o-Hai.pdf", 
      width = 14, height = 4.3*2.1, scale = 2,
      output_dir = plot_output_dir)

# ------------------------------------------------------------------------------
# Braunschweig H2O

err_plot_h2o_bs  <- function(method) {
    err_plot(method, "h2o", station_name = "Braunschweig", 
             results_tbl = results_tbl,
             method_label_xy = c(0, 1), 
             error_label_xyy = c(-1.8, -1.3, -1.5)) + 
    coord_cartesian(xlim = c(-1.8, 7), ylim = c(-1.5,1.3))
}

perf_h2o_bts     <- err_plot_h2o_bs("REA-BTS") 
perf_h2o_bcst    <- err_plot_h2o_bs("REA-LDB-BCST")
perf_h2o_q       <- err_plot_h2o_bs("QEA")
perf_h2o_ldb_bw  <- err_plot_h2o_bs("REA-LDB-BW" )
perf_h2o_db1_bw  <- err_plot_h2o_bs("REA-DB1-BW")
perf_h2o_ldb_BTS <- err_plot_h2o_bs("REA-LDB-BTS")
#
p_method_comparison_h2o_BS <- 
    (perf_h2o_bcst + (perf_h2o_ldb_bw + no_y_lab()) + (perf_h2o_q + no_y_lab())) /
    (perf_h2o_bts + (perf_h2o_db1_bw + no_y_lab()) + (perf_h2o_ldb_BTS + no_y_lab())) +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", 
          legend.box = "vertical", legend.box.just = "left") &
    guides(color = guide_legend(override.aes = list(size = 3)))


psave(p = p_method_comparison_h2o_BS, 
      name = "A1-comparison-against-REA-h2o-BS.pdf", 
      width = 14, height = 4.3*2.1, scale = 2,
      output_dir = plot_output_dir)

# ------------------------------------------------------------------------------
# Ts


perf_dt[, ts_flux := ts_flux * rho * Cp]
perf_dt[, ts_flux_q := ts_flux_q * rho * Cp]
perf_dt[, fe_ts_q := fe_ts_q * rho * Cp]
perf_dt[, fe_ts_ldb_bw := fe_ts_ldb_bw * rho * Cp]
perf_dt[, fe_ts_db1_bw := fe_ts_db1_bw * rho * Cp]

ts_method_label <- c(0, 40)
ts_error_label <- c(-100, -60, -70)
ts_coord  <- function() coord_cartesian(xlim = c(-100, 450), ylim = c(-70,60))




station_name <- "Braunschweig"
BS_perf_ts_q       <- err_plot("QEA", "ts", station_name, results_tbl = results_tbl, 
                               method_label_xy = ts_method_label, 
                               error_label_xyy = ts_error_label) 
BS_perf_ts_ldb_bw  <- err_plot("REA-LDB-BW", "ts", station_name, results_tbl = results_tbl,
                               method_label_xy = ts_method_label, 
                               error_label_xyy = ts_error_label)
BS_perf_ts_db1_bw  <- err_plot("REA-DB1-BW", "ts", station_name, results_tbl = results_tbl,
                               method_label_xy = ts_method_label, 
                               error_label_xyy = ts_error_label)
station_name <- "Hainich"
Hai_perf_ts_q       <- err_plot("QEA", "ts", station_name, results_tbl = results_tbl, 
                               method_label_xy = ts_method_label, 
                               error_label_xyy = ts_error_label)
Hai_perf_ts_ldb_bw  <- err_plot("REA-LDB-BW", "ts", station_name, results_tbl = results_tbl,
                               method_label_xy = ts_method_label, 
                               error_label_xyy = ts_error_label)
Hai_perf_ts_db1_bw  <- err_plot("REA-DB1-BW", "ts", station_name, results_tbl = results_tbl,
                               method_label_xy = ts_method_label, 
                               error_label_xyy = ts_error_label)


p_method_comparison_ts <- 
    (BS_perf_ts_ldb_bw + (BS_perf_ts_db1_bw + no_y_lab()) + (BS_perf_ts_q + no_y_lab() )) /
    (Hai_perf_ts_ldb_bw + (Hai_perf_ts_db1_bw + no_y_lab()) + (Hai_perf_ts_q + no_y_lab())) +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", 
          legend.box = "vertical", legend.box.just = "left") &
    guides(color = guide_legend(override.aes = list(size = 3))) & 
    labs(x = expression(paste("Ref ", H, " flux (", W~m^-2, ")")),
         y = expression(paste("Flux error ", " (", W~m^-2, ")")))  &
    ts_coord() 


psave(p = p_method_comparison_ts, 
      name = "A1-comparison-against-REA-H.pdf", 
      width = 14, height = 4.3*2.1, scale = 2,
      output_dir = plot_output_dir)


ggplot(perf_dt[station == "Hainich"][sample.t(time, '7d')]) +
    geom_line(aes(time, ts_flux))
