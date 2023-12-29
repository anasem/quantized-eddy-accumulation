# Additional plots that are not in the paper and show the normalized flux error
# for three different scalars CO2, H2O, and Ts 

source("src/plots/deps.R")
sim_errors <- readRDS(sim_errors_fn)

# Relative error shows similar dynamics between the three scalars
cat_colors <- c("#E3F2FDFF", "#BADEFAFF","#90CAF8FF","#41A5F4FF","#5F7D8BFF","#263238FF", 
                "#203134FF")
er_norm_breaks <- c(1e-9, 3e-3, 4e-3, 5e-3, 7e-3, 8e-3, 1e-2, 3e-2)
er_norm_labels <- c(round(er_norm_breaks*100, 2)[2:(length(er_norm_breaks))])

er_norm_plot <- 
    ggplot(sim_errors[directional_diffusion == TRUE & station == "BS"]) + 
        aes(qthreshold, full_scale_value) +
        geom_tile(aes(fill = cut(abs(Er_norm), 
                                 labels = er_norm_labels,
                                 breaks = er_norm_breaks)), col = 'white')  +
        scale_fill_manual("Max error %", values = cat_colors) +
        geom_hline(yintercept = c(3.8, 5.2), lty = 2, lwd = .2) +
        geom_abline(slope = 1/0.49, lty = 2, lwd = .2) +
        coord_equal(xlim = c(0, 5), ylim = c(2.5,6)) +
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
              legend.background = element_rect(fill = alpha("white", .8)))  +
    facet_wrap(~ scalar)


psave(p = er_norm_plot, name = "normalized-error-heatmap.png", device = "png",
      width = 8.5, height = 4,
      output_dir = plot_output_dir)


# ------------------------------------------------------------------------------
recommended_param_choice <- 
    (ggplot(sim_errors[(qthreshold/full_scale_value) > 0.35 & 
           full_scale_value %between% c(4, 5) & 
           directional_diffusion == TRUE]) +
        aes(qthreshold/ full_scale_value,Er_norm, col = scalar) +
        geom_point(size = .6) +
        geom_smooth(linewidth = .6, se = FALSE) +
        geom_hline(yintercept = 0.003, alpha = .3) +
        geom_vline(xintercept = c(0.55, 0.85), alpha = .3, lty = 2) +
        scale_color_manual(values = unname(mp(7,3,4, palette = palette_AE4))) +
        labs(x = "Scaled quantization threshold", y = "Normalized random error") +
        theme_ae23()) | 
    (ggplot(sim_errors[(qthreshold/full_scale_value) > 0.35 & 
           full_scale_value %between% c(4, 5) & 
           directional_diffusion == TRUE]) +
        aes(qthreshold/ full_scale_value,Er_slope, col = scalar) +
        geom_point(size = .6) +
        geom_smooth(linewidth = .6, se = FALSE) +
        geom_hline(yintercept = 0.000, alpha = .3) +
        geom_vline(xintercept = c(0.55, 0.85), alpha = .3, lty = 2) +
        scale_color_manual(values = unname(mp(7,3,4, palette = palette_AE4))) +
        labs(x = "Scaled quantization threshold", y = "Error slope",
             caption = "Optimal parameter choice, full scale value in range [4,5] σw 
             qunatization threshold in range [0.55, 0.85] of the full scale value") +
        theme_ae23()) 

recommended_param_choice <- recommended_param_choice  + 
    plot_layout(guides = "collect")  & legend_below() 

#
ggsave(file.path(plot_output_dir, "recommended-param-choice.png"), 
       plot = recommended_param_choice, 
       width = 900, height = 350, scale = 1, dpi = 100,
       device = "png", units = "px")

# Example of parameter choice which works very well
(ggplot(sim_errors[qthreshold > 1.5  & full_scale_value == 4.4 & 
                  directional_diffusion == TRUE]) +
    aes(qthreshold, Er_norm, col = scalar) +
    geom_point(size = .7) +
    geom_smooth(linewidth = .9, se = FALSE) +
    geom_hline(yintercept = 0.003, alpha = .3) +
    geom_vline(xintercept = c(2.3, 3.5), alpha = .3, lty = 2) +
    labs(x = "Quantization threshold", y = "Normalized uncertainty", 
         caption = "Full scale value: 4.4") +
    theme_ae23())  /
(ggplot(sim_errors[qthreshold > 1.5  & full_scale_value == 4.4 & 
                  directional_diffusion == TRUE]) +
    aes(qthreshold, Er_slope, col = scalar) +
    geom_point(size = .7) +
    geom_smooth(linewidth = .9, se = FALSE) +
    geom_hline(yintercept = 0.000, alpha = .3) +
    geom_vline(xintercept = c(2.3, 3.5), alpha = .3, lty = 2) +
    labs(x = "Quantization threshold", y = "Error slope", 
         caption = "Full scale value: 4.4") +
    theme_ae23())


# The mean of these as an indicator of the random error in the flux
sim_errors[full_scale_value %between%  c(4,5) & 
           (qthreshold / full_scale_value) %between% c(.6, .8) & 
           directional_diffusion == TRUE,  
           .(Normalized_error = mean(Er_norm)), by = scalar ]

#    scalar Normalized_error
# 1:    co2      0.002826987
# 2:    h2o      0.002714723
# 3:     ts      0.002544396




