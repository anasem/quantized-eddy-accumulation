source("src/plots/deps.R")
source("src/config/default.R")

perf_dt         <- readRDS(sim_perf_eval_fn)

# Generate a data.table for the theoretical error of correlation coefficient
perf_dt[station == "Braunschweig", median(N_tot)]
perf_dt_thoery <- data.table(r_wTs = seq(-0.6, .7, by = 0.01), N_tot = 18000, QC = "OK")
perf_dt_thoery[, ndfe_theory := (1 - r_wTs^2) / sqrt(N_tot)]

pt_size <- .9 
pt_alpha <- 0.5

# Non-dimensional flux errors for 3 scalars
ndfe_qea_mean <- perf_dt[QC == "OK", mean(c(sd(ndfe_co2_q), sd(ndfe_h2o_q), sd(ndfe_Ts_q)))]
nondim_uncert_qea <-
    ggplot(perf_dt[station == "Braunschweig"]) +
        aes(r_wTs, ndfe_co2_q, col = "CO2", shape = QC) +
        geom_rect(xmin = ndfe_qea_mean/ 0.1, xmax = ndfe_qea_mean/ -0.1,
                  ymin = -Inf, ymax = Inf, fill = "gray92", col = NA) +
        geom_hline(yintercept = 0, alpha = 0.3) +
        geom_point(aes(y = ndfe_h2o_q, col = "H2O"), alpha = pt_alpha, size = pt_size) +
        geom_point(aes(y = ndfe_Ts_q, col = "Ts"), alpha = pt_alpha, size = pt_size) +
        geom_point(alpha = pt_alpha, size = pt_size) +
        geom_line(data = perf_dt_thoery, aes(y = 2*ndfe_theory), linetype = 1, 
                  alpha = 0.9, show.legend = FALSE, 
                  col = mp(3, palette = palette_AE4) ) +
        geom_line(data = perf_dt_thoery, aes(y = -2*ndfe_theory), linetype = 1, 
                  alpha = 0.9, show.legend = FALSE,
                  col = mp(3, palette = palette_AE4) ) +
        geom_hline(yintercept = ndfe_qea_mean * c(-2,2), alpha = 0.5, lty = 2) +
        geom_vline(xintercept = 0, alpha = 0.1) +
        ylim(-0.03, 0.03) +
        scale_color_manual("Scalar", values = mp(c(4,9,3), palette = palette_AE4),
                           labels = c(expression(CO[2]), expression(H[2]*O),expression(theta) )) +
        geom_text(inherit.aes = FALSE, x = 0.2, y = 0.027, 
                  label = paste("bold(QEA)~~{'non-dim'}~uncert.~", round(ndfe_qea_mean, 3)), 
                  check_overlap = TRUE, parse = TRUE, size = 4) +
        labs(
          x = expression(rho[w~theta]),  
          y = "Normalized error"
        ) +
        scale_shape_manual(name = "Quality",  values = c(3,16))+
        theme_ae23() 

ndfe_db1_bw_mean <- 
    perf_dt[QC == "OK" , 
            mean(c(sd(ndfe_co2_db1_bw), sd(ndfe_h2o_db1_bw), sd(ndfe_ts_db1_bw)))]
#
nodim_uncert_db1_bw <-
    ggplot(perf_dt[station == "Braunschweig"]) +
        aes(r_wTs, ndfe_co2_db1_bw, col = "CO2", shape = QC) +
        geom_rect(xmin = ndfe_db1_bw_mean / 0.1, xmax = ndfe_db1_bw_mean / -0.1,
                  ymin = -Inf, ymax = Inf, fill = "gray92", col = NA) +
        geom_point(aes(y = ndfe_h2o_db1_bw , col = "H2O"), alpha = .5, size = pt_size) +
        geom_point(aes(y = ndfe_ts_db1_bw, col = "Ts"), alpha = .5, size = pt_size) +
        geom_point(alpha = .5, size = pt_size) +
        geom_line(data = perf_dt_thoery, aes(y = 2*ndfe_theory), linetype = 1, 
                  alpha = 0.9, show.legend = FALSE, 
                  col = mp(3, palette = palette_AE4) ) +
        geom_line(data = perf_dt_thoery, aes(y = -2*ndfe_theory), linetype = 1, 
                  alpha = 0.9, show.legend = FALSE,
                  col = mp(3, palette = palette_AE4) ) +
        geom_hline(yintercept = ndfe_db1_bw_mean * c(-2,2), alpha = 0.5, lty = 2) +
        geom_vline(xintercept = 0, alpha = 0.1) +
        # Make shaded region
        ylim(-0.3, 0.3) +
        scale_color_manual("Scalar", values = mp(c(4,9,3), palette = palette_AE4),
                           labels = c(expression(CO[2]), expression(H[2]*O),expression(theta) )) +
        geom_text(inherit.aes = FALSE, x = 0.2, y = 0.27, 
                  label = paste("bold(REA)~~{'non-dim.'}~uncert.~", round(ndfe_db1_bw_mean, 3)), 
                  check_overlap = TRUE, parse = TRUE, size = 4) +
        scale_shape_manual(name = "Quality",  values = c(3,16))+
        labs(
          x = expression(rho[w~theta]),  
          y = "Normalized error"
        ) +
        theme_ae23() 


p_method_nondim_uncertainty <- 
    ((nondim_uncert_qea +theme(axis.title.x = element_blank()) )/ nodim_uncert_db1_bw) +
        plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom", legend.box.just = "left") &
        guides(color = guide_legend(override.aes = list(size = 2)), 
               shape = guide_legend(override.aes = list(size = 2))) 

psave(p = p_method_nondim_uncertainty,
      name = "nondim-uncertainty.pdf", 
      width = 6, height = 4.3 * 2.1, scale = 2,
      output_dir = plot_output_dir)

