source("src/plots/deps.R")

sim_errors <- readRDS(sim_errors_fn)

cat_colors <- c("#E3F2FDFF", "#BADEFAFF","#90CAF8FF","#41A5F4FF","#5F7D8BFF","#263238FF")

slope_breaks <- c(1e-9, 1e-3, 2e-3, 5e-3, 1e-2, 2e-2, 5e-2)

parameter_heatmap_slope <- 
    ggplot(sim_errors[directional_diffusion == TRUE & Er_slope < 5e-2 &
           station == "BS" & scalar == "co2"]) + 
        aes(qthreshold, full_scale_value) +
        geom_tile(aes(fill = cut(abs(Er_slope), 
                                 labels = c(round(slope_breaks*100, 2)[2:(length(slope_breaks))]),
                                 breaks = slope_breaks)), col = 'white')  +
        scale_fill_manual("Max error %", values = cat_colors) +
        geom_hline(yintercept = 2.3, col = "white", lty = 2) +
        coord_equal(xlim = c(0, 3), ylim = c(2,5)) +
        theme_ae23() + 
        labs( x = "Quantization threshold", y = "Full scale value") + 
        theme(legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.margin = margin(1,1,1,1),
              legend.box.margin = margin(1,1,1,1),
              legend.title = element_text(size = 8, face = "bold",
                                          margin = margin(0,5,0,0)),
              legend.text= element_text(size = 8, margin = margin(0,7,0,0)),
              legend.justification = c(0.5, 0),
              legend.spacing.x = unit(1,'mm'),
              legend.key.size = unit(4, "mm"),
              legend.background = element_rect(fill = alpha("white", .8))) 

er_sd_breaks <-  c(0,  0.01, 0.015, 0.02,  0.03, 0.035, 0.05)
#
parameter_heatmap_sd <- 
    ggplot(sim_errors[directional_diffusion == TRUE & station == "BS" & scalar == "co2" & 
           Er_rmse/flux_sd < 0.05]) + 
        aes(qthreshold, full_scale_value, fill = cut(Er_rmse/flux_sd, 
                                               labels = c(round(er_sd_breaks*100, 3)[2:(length(er_sd_breaks))]),
                                               breaks = er_sd_breaks)) +
        geom_tile(col = 'white')   +
        scale_fill_manual("Max RMSE / SD %", values = cat_colors) +
        geom_hline(yintercept = 2.3, col = "white", lty = 2) +
        coord_equal(xlim = c(0, 3), ylim = c(2,5)) +
        theme_ae23() +
        labs( x = "Quantization threshold", y = "Full scale value") + 
        theme(legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.margin = margin(1,1,1,1),
              legend.box.margin = margin(1,1,1,1),
              legend.title = element_text(size = 8, face = "bold",
                                          margin = margin(0,5,0,0)),
              legend.text= element_text(size = 8, margin = margin(0,7,0,0)),
              legend.justification = c(0.5, 0),
              legend.spacing.x = unit(1,'mm'),
              legend.key.size = unit(4, "mm"),
              legend.background = element_rect(fill = alpha("white", .8))) + 
        guides(fill = guide_legend (nrow = 2))

parameter_heatmap <- 
    parameter_heatmap_slope + parameter_heatmap_sd +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  

psave(p = parameter_heatmap, name = "parameter-heatmap.png", device = "png",
      width = 10, height = 6, 
      output_dir = plot_output_dir)


