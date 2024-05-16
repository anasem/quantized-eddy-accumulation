source("src/plots/deps.R")
source("src/config/default.R")

sim_errors <- readRDS(sim_errors_fn)

cat_colors <- c("#E3F2FDFF", "#BADEFAFF","#90CAF8FF","#41A5F4FF",
                "#5F7D8BFF","#263238FF")

slope_breaks <- c(1e-9, 1e-3, 2e-3, 5e-3, 1e-2, 2e-2, 5e-2)

coord_grid <- function() coord_equal(xlim = c(0.1, 3.2), ylim = c(2.3,5.2))

theme_heatmap <- function() {
        theme(legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.margin = margin(1,1,1,1),
              legend.box.margin = margin(1,1,1,1),
              legend.title = element_text(size = 7, face = "bold",
                                          margin = margin(0,5,0,0)),
              legend.text= element_text(size = 8, margin = margin(0,3,0,1)),
              legend.justification = c(0.3, 0),
              legend.spacing.x = unit(0.3,'mm'),
              legend.key.spacing.x = unit(1.0, "mm"), 
              legend.key.spacing.y = unit(0.5, "mm"),
              legend.key.size = unit(4, "mm"),
              legend.background = element_rect(fill = alpha("white", .8))) 
}

parameter_heatmap_slope <- 
    ggplot(sim_errors[directional_diffusion == FALSE & Er_slope < 5e-2  & scalar == "co2"]) + 
        aes(qthreshold, full_scale_value) +
        geom_tile(aes(fill = cut(abs(Er_slope), 
                                 labels = c(round(slope_breaks*100, 2)[2:(length(slope_breaks))]),
                                 breaks = slope_breaks)), col = 'white')  +
        scale_fill_manual("Max error %", values = cat_colors) +
        geom_hline(yintercept = 2.8, col = "white", lty = 2) +
        geom_abline(intercept = 0, slope = 2, col = "gray40", lty = 1, alpha = .35) +
        theme_ae23() + 
        coord_grid() +
        labs( x = "Quantization threshold", y = "Full scale value") + 
        guides(fill = guide_legend (nrow = 2)) +
        theme_heatmap() 


er_sd_breaks <-  c(0,  0.004, 0.005, 0.006,  0.01, 0.02, 0.1)
#
parameter_heatmap_sd <- 
    ggplot(sim_errors[directional_diffusion == FALSE & scalar == "co2" & u_norm < 0.1]) + 
        aes(qthreshold, full_scale_value, fill = cut(u_norm, 
                                               labels = c(round(er_sd_breaks, 3)[2:(length(er_sd_breaks))]),
                                               breaks = er_sd_breaks)) +
        geom_tile(col = 'white')   +
        scale_fill_manual("Non-dim.\nuncert.", values = cat_colors) +
        geom_hline(yintercept = 2.8, col = "white", lty = 2) +
        geom_abline(intercept = 0, slope = 2, col = "gray40", lty = 1, alpha = .35) +
        theme_ae23() +
        coord_grid() +
        labs( x = "Quantization threshold", y = "Full scale value") +  
        guides(fill = guide_legend (nrow = 2)) +
        theme_heatmap() 
#
parameter_heatmap <- 
    parameter_heatmap_slope + parameter_heatmap_sd +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  


psave(p = parameter_heatmap, 
      name = "A3-parameter-heatmap-directional-diffusion-BS.pdf",
      width = 10, height = 6, 
      output_dir = plot_output_dir)

#-------------------------------------------------------------------------------
source("src/config/hai.R")
sim_errors <- readRDS(sim_errors_fn)

cat_colors <- c("#E3F2FDFF", "#BADEFAFF","#90CAF8FF","#41A5F4FF",
                "#5F7D8BFF","#263238FF")


coord_grid <- function() coord_equal(xlim = c(0.1, 3.2), ylim = c(2.3,5.2))

theme_heatmap <- function() {
        theme(legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.margin = margin(1,1,1,1),
              legend.box.margin = margin(1,1,1,1),
              legend.title = element_text(size = 7, face = "bold",
                                          margin = margin(0,5,0,0)),
              legend.text= element_text(size = 8, margin = margin(0,3,0,1)),
              legend.justification = c(0.3, 0),
              legend.spacing.x = unit(0.3,'mm'),
              legend.key.spacing.x = unit(1.0, "mm"), 
              legend.key.spacing.y = unit(0.5, "mm"),
              legend.key.size = unit(4, "mm"),
              legend.background = element_rect(fill = alpha("white", .8))) 
}

slope_breaks <- c(1e-9, 2e-4, 2e-3, 1e-3, 1e-2, 2e-2, 5e-2)
parameter_heatmap_slope <- 
    ggplot(sim_errors[directional_diffusion == TRUE & Er_slope < 5e-2  & scalar == "co2"]) + 
        aes(qthreshold, full_scale_value) +
        geom_tile(aes(fill = cut(abs(Er_slope), 
                                 labels = c(round(slope_breaks*100, 2)[2:(length(slope_breaks))]),
                                 breaks = slope_breaks)), col = 'white')  +
        scale_fill_manual("Max error %", values = cat_colors) +
        geom_hline(yintercept = 2.8, col = "white", lty = 2) +
        geom_abline(intercept = 0, slope = 2, col = "gray40", lty = 1, alpha = .35) +
        theme_ae23() + 
        coord_grid() +
        labs( x = "Quantization threshold", y = "Full scale value") + 
        guides(fill = guide_legend (nrow = 2)) +
        theme_heatmap() 


er_sd_breaks <-  c(0,  0.004, 0.005, 0.006,  0.01, 0.02, 0.1)
#
parameter_heatmap_sd <- 
    ggplot(sim_errors[directional_diffusion == TRUE & scalar == "co2" & u_norm < 0.1]) + 
        aes(qthreshold, full_scale_value, fill = cut(u_norm, 
                                               labels = c(round(er_sd_breaks, 3)[2:(length(er_sd_breaks))]),
                                               breaks = er_sd_breaks)) +
        geom_tile(col = 'white')   +
        scale_fill_manual("Non-dim.\nuncert.", values = cat_colors) +
        geom_hline(yintercept = 2.8, col = "white", lty = 2) +
        geom_abline(intercept = 0, slope = 2, col = "gray40", lty = 1, alpha = .35) +
        theme_ae23() +
        coord_grid() +
        labs( x = "Quantization threshold", y = "Full scale value") +  
        guides(fill = guide_legend (nrow = 2)) +
        theme_heatmap() 
#
parameter_heatmap <- 
    parameter_heatmap_slope + parameter_heatmap_sd +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  

psave(p = parameter_heatmap, name = "A3-parameter-heatmap--Hai.pdf",
      width = 10, height = 6, 
      output_dir = plot_output_dir)
