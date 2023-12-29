source("src/plots/deps.R")

# Load simulation results
perf_eval_sim <- readRDS(sim_perf_eval_fn)
results_tbl   <- readRDS(results_tbl_fn)

co2_method_name.x  <- -30
co2_method_name.y  <- 7
point_size         <- 1.1
diurnal_ln_col     <- mp(8, palette =  palette_AE4)
text_color         <- mp(8, palette =  palette_AE4)
lm_lwd             <- 0.7
mean_lnw           <- 0.8


coord_cartesian_co2 <- function() coord_cartesian(ylim = c(-9, 9))  
perf_eval_sim[, zl_cat := cut_zL3(zL)]

perf_eval_sim <- perf_eval_sim[order(zl_cat)]

p_co2_bcst <- 
ggplot(perf_eval_sim[station == "BS"]) + 
    aes(co2_flux, fe_co2_ldb_bcst, col = zl_cat, shape = zl_cat)+
    geom_hline(yintercept = 0, alpha = 0.3) + 
    geom_point(alpha = .8, size = point_size) +
    geom_smooth(method = "lm", se = FALSE, show.legend= FALSE, linewidth = lm_lwd) +
    coord_cartesian_co2() +
    theme_ae23() +
    stability_color_scale() +
    geom_text(x = co2_method_name.x, y = co2_method_name.y, 
              label = "bold(REA)~~beta[ts-med]~method", 
              inherit.aes = FALSE, check_overlap = TRUE,
              hjust = 0,
              vjust = "left",
              color = text_color,
              family = "Noto sans", parse = TRUE, size = 3) +
    method_stats(results_tbl, "REA-LDB-BCST", "co2", metric = "mean_bias", x = -45, y = -6.5) +
    method_stats(results_tbl, "REA-LDB-BCST", "co2", metric = "slope", x = -45, y = -8) +
    method_stats(results_tbl, "REA-LDB-BCST", "co2", metric = "rmse", x = -45, y = -9) +
    scale_shape_manual(name = "Stability class",  values = c(16,15,16))+
    labs(x = expression(paste("Ref ", CO[2], " flux (", mu, mol, ~m^-2,~s^-1, ")")),
         y = expression(paste("Flux error ", " (", mu, mol, ~m^-2,~s^-1, ")"))) 

p_co2_bts <- 
ggplot(perf_eval_sim[station == "BS"]) + 
    aes(co2_flux, fe_co2_ldb_bts, col = zl_cat, shape = zl_cat)+
    geom_hline(yintercept = 0, alpha = 0.3) + 
    geom_point(alpha = .8, size = point_size) +
    geom_smooth(method = "lm", se = FALSE, show.legend= FALSE, linewidth = lm_lwd) +
    coord_cartesian_co2() +
    theme_ae23() +
    stability_color_scale() +
    geom_text(x = co2_method_name.x, y = co2_method_name.y, 
              label = "bold(REA)~~beta[Ts]~method", 
              inherit.aes = FALSE, check_overlap = TRUE,
              hjust = 0,
              vjust = "left",
              color = text_color,
              family = "Noto sans", parse = TRUE, size = 3) +
    method_stats(results_tbl, "REA-LDB-BTS", "co2", metric = "mean_bias", x = -45, y = -6.5) +
    method_stats(results_tbl, "REA-LDB-BTS", "co2", metric = "slope", x = -45, y = -8) +
    method_stats(results_tbl, "REA-LDB-BTS", "co2", metric = "rmse", x = -45, y = -9) +
    scale_shape_manual(name = "Stability class",  values = c(16,15,16))+
    labs(x = expression(paste("Ref ", CO[2], " flux (", mu, mol, ~m^-2,~s^-1, ")")),
         y = expression(paste("Flux error ", " (", mu, mol, ~m^-2,~s^-1, ")"))) 

p_co2_bw <- 
ggplot(perf_eval_sim[station == "BS"]) + 
    aes(co2_flux, fe_co2_ldb_bw, col = zl_cat, shape = zl_cat)+
    geom_hline(yintercept = 0, alpha = 0.3) + 
    geom_point(alpha = .8, size = point_size) +
    geom_smooth(method = "lm", se = FALSE, show.legend= FALSE, linewidth = lm_lwd) +
    geom_text(x = co2_method_name.x, y = co2_method_name.y, 
              label = "bold(REA)~~beta[w]~method", 
              inherit.aes = FALSE, check_overlap = TRUE,
              hjust = 0,
              vjust = "left",
              color = text_color,
              family = "Noto sans", parse = TRUE, size = 3) +
    method_stats(results_tbl, "REA-LDB-BW", "co2", metric = "mean_bias", x = -45, y = -6.5) +
    method_stats(results_tbl, "REA-LDB-BW", "co2", metric = "slope", x = -45, y = -8) +
    method_stats(results_tbl, "REA-LDB-BW", "co2", metric = "rmse", x = -45, y = -9) +
    coord_cartesian_co2() +
    theme_ae23() +
    stability_color_scale() +
    scale_shape_manual(name = "Stability class",  values = c(16,15,16))+
    labs(x = expression(paste("Ref ", CO[2], " flux (", mu, mol, ~m^-2,~s^-1, ")")),
         y = "")

p_co2_q <- 
ggplot(perf_eval_sim[station == "BS"]) + 
    aes(co2_flux, fe_co2_q, col = zl_cat, shape = zl_cat)+
    geom_hline(yintercept = 0, alpha = 0.3) + 
    geom_point(alpha = .8, size = point_size) +
    geom_smooth(method = "lm", se = FALSE, show.legend= FALSE, linewidth = lm_lwd) +
    geom_text(x = co2_method_name.x, y = co2_method_name.y, 
              label = "New~bold(QEA)~~method", 
              inherit.aes = FALSE, check_overlap = TRUE,
              hjust = 0,
              vjust = "left",
              color = text_color,
              family = "Noto sans", parse = TRUE, size = 3) +
    method_stats(results_tbl, "QEA", "co2", metric = "mean_bias", x = -45, y = -6.5) +
    method_stats(results_tbl, "QEA", "co2", metric = "slope", x = -45, y = -8) +
    method_stats(results_tbl, "QEA", "co2", metric = "rmse", x = -45, y = -9) +
    coord_cartesian_co2() +
    theme_ae23() +
    stability_color_scale() +
    scale_shape_manual(name = "Stability class",  values = c(16,15,16))+
    labs(x = expression(paste("Ref ", CO[2], " flux (", mu, mol, ~m^-2,~s^-1, ")")),
         y = "")

diurnal_qea <- 
    ggplot(perf_eval_sim[station == "BS"]) + 
        aes(x = hour(time) + round(minute(time)/30)*.5) +
        # Bands
        stat_summary(mapping = , aes(y = fe_co2_q),
                 geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 alpha=.2, 
                 size = .2,
                 linetype = 3, show.legend = FALSE)  +
        # Mean line
        stat_summary(mapping = aes(y = fe_co2_q), 
                 fun = mean, geom = "line", 
                 color = diurnal_ln_col, 
                 fun.args = list(na.rm = TRUE), size = mean_lnw, alpha = .9)  +
        labs(x = "Local time",
             y = "") +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian(ylim = c(-5, 5)) + 
        geom_hline(yintercept = 0, alpha = 0.3) + 
        theme_ae23() 

dirunal_ldb_bts <- 
    ggplot(perf_eval_sim[station == "BS"]) + 
        aes(x = hour(time) + round(minute(time)/30)*.5) +
        # Bands RE
        stat_summary(mapping = aes(y = fe_co2_ldb_bts),
                 geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 linewidth = .2,
                 fill = mp(7, palette =  palette_AE4),
                 alpha=.2, 
                 linetype = 3, show.legend = FALSE)  +
        # Mean line REA
        stat_summary(mapping = aes(y = fe_co2_ldb_bts), 
                 fun = mean, geom = "line", 
                 color = diurnal_ln_col, 
                 fun.args = list(na.rm = TRUE), linewidth = mean_lnw, alpha = .9) +
        geom_hline(yintercept = 0, alpha = 0.3) + 
        labs(x = "Local time",
            y = expression(paste(CO[2], " mean flux bias (", mu, mol, ~m^-2,~s^-1, ")"))) +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian(ylim = c(-5, 5)) + 
        theme_ae23() 

diurnal_ldb_bcst <- 
    ggplot(perf_eval_sim[station == "BS"]) + 
        aes(x = hour(time) + round(minute(time)/30)*.5) +
        # Bands RE
        stat_summary(mapping = aes(y = fe_co2_ldb_bcst),
                 geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 linewidth = .2,
                 alpha=.2, 
                 linetype = 3, show.legend = FALSE)  +
        # Mean line REA
        stat_summary(mapping = aes(y = fe_co2_ldb_bcst), 
                 fun = mean, geom = "line", 
                 fun.args = list(na.rm = TRUE), 
                 linewidth = mean_lnw, 
                 color = diurnal_ln_col, 
                 alpha = .9) +
        geom_hline(yintercept = 0, alpha = 0.3, linewidth = .5) + 
        labs(x = "Local time",
            y = expression(paste(CO[2], " flux bias (", mu, mol, ~m^-2,~s^-1, ")"))) +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian(ylim = c(-5, 5)) + 
        theme_ae23() 


diurnal_ldb_bw <- 
    ggplot(perf_eval_sim[station == "BS"]) + 
        aes(x = hour(time) + round(minute(time)/30)*.5) +
        # Bands REA
        stat_summary(mapping = aes(y = fe_co2_ldb_bw),
                 geom="ribbon", fun.data=mean_cl_normal, 
                 fun.args=list(conf.int=0.95), 
                 linewidth = .2,
                 alpha=.2, 
                 linetype = 3, show.legend = FALSE)  +
        # Mean line REA
        stat_summary(mapping = aes(y = fe_co2_ldb_bw), 
                 fun = mean, geom = "line", 
                 color = diurnal_ln_col, 
                 fun.args = list(na.rm = TRUE), linewidth = mean_lnw,
                 alpha = .9)  +
        geom_hline(yintercept = 0, alpha = 0.3) + 
        labs(x = "Local time",
             y = "") +
        scale_x_continuous(expand = c(0,0), minor_breaks = NULL)  +
        coord_cartesian(ylim = c(-5, 5))  +
        theme_ae23() 

p_method_comparison_co2_w_diurnal <- 
    (p_co2_bcst + p_co2_bw + p_co2_q)  /
    (diurnal_ldb_bcst + diurnal_ldb_bw + diurnal_qea) +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom", 
          legend.box = "vertical", legend.box.just = "left") &
    guides(color = guide_legend(override.aes = list(size = 3)))


psave(p = p_method_comparison_co2_w_diurnal, 
      name = "comparison-against-REA-co2-and-diurnal.pdf", 
      width = 14, height = 4.3*2.0, scale = 1.9,
      output_dir = plot_output_dir)


