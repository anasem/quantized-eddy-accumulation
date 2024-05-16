source("src/plots/deps.R")
source("src/config/default.R")

# Load simulation results
perf_dt         <- readRDS(sim_perf_eval_fn)
results_tbl     <- readRDS(perf_metrics_fn)


co2_method_name.x  <- -30
co2_method_name.y  <- 7
point_size         <- 1.1
diurnal_ln_col     <- mp(8, palette =  palette_AE4)
text_color         <- mp(8, palette =  palette_AE4)
mean_lnw           <- 0.8


perf_dt <- perf_dt[co2_flux < 10]

co2_flux_labs <-  function() {
    labs(x = expression(paste("Ref ", CO[2], " flux (", mu, mol, ~m^-2,~s^-1, ")")),
         y = expression(paste("Flux error ", " (", mu, mol, ~m^-2,~s^-1, ")"))) 
}

h2o_flux_labs <-  function() {
    labs(x = expression(paste("Ref ", H[2], "O flux (", m, mol, ~m^-2,~s^-1, ")")),
         y = expression(paste("Flux error ", " (", m, mol, ~m^-2,~s^-1, ")"))) 
}


perf_dt <- perf_dt[order(zl_cat)]
perf_dt[, QC := "Low"]
perf_dt[Rs_wc < 0.3 & ustar > 0.1, QC := "OK"]

# ------------------------------------------------------------------------------

co2_ylim <- c(-12, 12)
station_name <- "Braunschweig"
perf_co2_bts_BS     <- err_plot("REA-BTS", "co2", station_name,  ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_bcst_BS    <- err_plot("REA-LDB-BCST", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_q_BS       <- err_plot("QEA", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_ldb_bw_BS  <- err_plot("REA-LDB-BW", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)
perf_co2_db1_bw_BS  <- err_plot("REA-DB1-BW", "co2", station_name, ylim = co2_ylim, results_tbl = results_tbl)


# CO2 diurnal plots ------------------------------------------------------------

perf_dt[, local_hour := hour(time) + round(minute(time)/30)*.5]
coord_cartesian_co2_diurnal <- function() coord_cartesian(ylim = c(-1.2, 1.8))

diurnal_qea <- 
    ggplot(perf_dt[QC == "OK" & station == "Braunschweig" ]) + 
            aes(x = local_hour, y = fe_co2_q) +
        # Bands 
        stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 linewidth = .2,
                 alpha=.2, 
                 linetype = 3, show.legend = FALSE)  +
        # Mean line 
        stat_summary(fun = mean, geom = "line", 
                 fun.args = list(na.rm = TRUE), 
                 linewidth = mean_lnw, 
                 alpha = .9) +
        geom_hline(yintercept = 0, alpha = 0.3, linewidth = .5) + 
        labs(x = "Local time",
            y = expression(paste(CO[2], " flux bias (", mu, mol, ~m^-2,~s^-1, ")"))) +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian_co2_diurnal() +
        scale_fill_manual(values = mp(c(4,5), palette = palette_AE4)) +
        scale_color_manual(values = mp(c(4,5), palette = palette_AE4))  +
        theme_ae23() 


# LDB-BSCT
diurnal_ldb_bcst <- 
    ggplot(perf_dt[QC == "OK" & station == "Braunschweig" ]) + 
        aes(x = local_hour, y = fe_co2_ldb_bcst) +
        # Bands 
        stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 linewidth = .2,
                 alpha=.2, 
                 linetype = 3, show.legend = FALSE)  +
        # Mean line 
        stat_summary(fun = mean, geom = "line", 
                 fun.args = list(na.rm = TRUE), 
                 linewidth = mean_lnw, 
                 alpha = .9) +
        geom_hline(yintercept = 0, alpha = 0.3, linewidth = .5) + 
        labs(x = "Local time",
            y = expression(paste(CO[2], " flux bias (", mu, mol, ~m^-2,~s^-1, ")"))) +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian_co2_diurnal() +
        scale_fill_manual(values = mp(c(4,5), palette = palette_AE4)) +
        scale_color_manual(values = mp(c(4,5), palette = palette_AE4))  +
        theme_ae23() 

# DB1-BW
diurnal_db1_bw <-
    ggplot(perf_dt[QC == "OK" & station == "Braunschweig"]) + 
        aes(x = local_hour, y = fe_co2_db1_bw) +
        # Bands RE
        stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 linewidth = .2,
                 alpha=.2, 
                 linetype = 3, show.legend = FALSE)  +
        # Mean line REA
        stat_summary(fun = mean, geom = "line", 
                 fun.args = list(na.rm = TRUE), 
                 linewidth = mean_lnw, 
                 alpha = .9) +
        geom_hline(yintercept = 0, alpha = 0.3, linewidth = .5) + 
        labs(x = "Local time",
            y = expression(paste(CO[2], " flux bias (", mu, mol, ~m^-2,~s^-1, ")"))) +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian_co2_diurnal() +
        scale_fill_manual(values = mp(c(4,5), palette = palette_AE4)) +
        scale_color_manual(values = mp(c(4,5), palette = palette_AE4))  +
        theme_ae23() 


diurnal_ldb_bw <-
    ggplot(perf_dt[QC == "OK" & station == "Braunschweig"]) + 
        aes(x = local_hour, y = fe_co2_ldb_bw) +
        # Bands RE
        stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 linewidth = .2,
                 alpha=.2, 
                 linetype = 3, show.legend = FALSE)  +
        # Mean line REA
        stat_summary(fun = mean, geom = "line", 
                 fun.args = list(na.rm = TRUE), 
                 linewidth = mean_lnw, 
                 alpha = .9) +
        geom_hline(yintercept = 0, alpha = 0.3, linewidth = .5) + 
        labs(x = "Local time",
            y = expression(paste(CO[2], " flux bias (", mu, mol, ~m^-2,~s^-1, ")"))) +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian_co2_diurnal() +
        scale_fill_manual(values = mp(c(4,5), palette = palette_AE4)) +
        scale_color_manual(values = mp(c(4,5), palette = palette_AE4))  +
        theme_ae23() 




# Saving -----------------------------------------------------------------------

p_method_comparison_co2_w_diurnal <- 
    (perf_co2_bcst_BS + (perf_co2_ldb_bw_BS + no_y_lab()) + (perf_co2_q_BS  + no_y_lab())) /
    (diurnal_ldb_bcst + (diurnal_ldb_bw   + no_y_lab())+ (diurnal_qea + no_y_lab())) +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", 
          legend.box = "vertical", legend.box.just = "left") &
    guides(color = guide_legend(override.aes = list(size = 3)))


psave(p = p_method_comparison_co2_w_diurnal, 
      name = "comparison-against-REA-co2-and-diurnal.pdf", 
      width = 14, height = 4.3*2.1, scale = 2,
      output_dir = plot_output_dir)

