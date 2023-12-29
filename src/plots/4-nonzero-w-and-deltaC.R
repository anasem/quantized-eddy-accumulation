source("src/plots/deps.R")

fsv_colors <- unname(mp(1,3,2,5, palette = palette_AE4))
nzero_w_err_dt <- readRDS(nzero_w_err_fn)

# how much the error is reduced
noznero_w_reduction_p <- 
    ggplot(nzero_w_err_dt[full_scale_value %in% c(3, 4,5, 6) & 
           directional_diffusion == TRUE & station == "BS"]) + 
        geom_hline(yintercept =  1, lty = 2, col = "gray") +
        geom_vline(xintercept =  .5, lty = 1, col = "gray") +
        geom_line(aes(qthreshold/full_scale_value, 
                      1/w_emp_ratio, col = as.factor(full_scale_value)))  +
        theme_ae23() +
        coord_cartesian(xlim = c(0, 0.7)) +
        scale_color_manual("Full scale value", values = fsv_colors) +
        labs(x = "Scaled quantization threshold", y = "Error reduction factor")

C_maximization_p <- 
    ggplot(nzero_w_err_dt[full_scale_value %in% c(3, 4,5, 6) & 
           directional_diffusion == TRUE & station == "BS"]) + 
        geom_hline(yintercept =  c(1, 1.5), lty = 2, col = "gray") +
        geom_vline(xintercept =  .5, lty = 1, col = "gray") +
        geom_line(aes(qthreshold/full_scale_value, 
                      C_emp_slope, col = as.factor(full_scale_value))) +  
        theme_ae23() +
        coord_cartesian(xlim = c(0, 0.7)) +
        scale_color_manual("Full scale value", values = fsv_colors) +
        labs(x = "Scaled quantization threshold", 
             y = expression(Delta~C~increase~factor), parse = TRUE)

w_C_tradeoff_plot <- 
    (noznero_w_reduction_p / C_maximization_p)  +
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  +
    plot_layout(guides = "collect")  & legend_below() 


psave(p = w_C_tradeoff_plot, name = "w-C-reduction-maximization.png", 
      width = 4.3, height = 7.3, scale = 2,  device = png, 
      output_dir = plot_output_dir)


