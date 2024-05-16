# Helper functions, themes and styling options

palette_AE4 <- c("darkblue" = "#2154A6",
                 "darkred" = "#B32424",
                 "brightred" = "#FF4040",
                 "skyblue" = "#39A1E6",
                 "beige" = "#998563",
                 "lightgrey" = "#D4C7C7",
                 "charcoal" = "#544F4F",
                 "darkgrey" = "#363232")

# Main palette easy access
mp <- function(..., palette = pal_AE1) {
    chosen_colors <- list(...)

    main_palette <- palette

    if (length(chosen_colors) == 0) {
        ggplot(data.frame(num = 1:length(main_palette),
                          colorval = names(main_palette),
                          y = 10)) + 
            geom_col(aes(x = num, y = y, fill = colorval)) + 
            geom_text(aes(x = num, y = 5, label = num))+
            scale_fill_manual(values = main_palette)
    } else {
        return(unname(main_palette[unlist(chosen_colors)]))
    }
}

geom_1to1  <- function(intercept = 0, ...) {
    geom_abline(slope = 1, intercept = intercept, ...)
}

# My theme
theme_ae23 <- function() {
  return (theme_light() + 
    theme(text = element_text(family="Source Sans Pro", size = 10),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.subtitle = element_text(color = "gray30"),
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(color = "gray30"),
            axis.title.x = element_text(size = 12, 
                                        color = "gray30",
                                        margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size = 12, 
                                        color = "gray30",
                                        margin = margin(t = 0, r = 5, b = 0, l = 0)),
            axis.text.x= element_text(size = 12),
            axis.text.y = element_text(size = 12),
            # plot.margin = grid::unit(c(1,4,1,1), "mm"),
            legend.position = "inside",
            legend.position.inside = c(.99, .01),
            legend.box = "horizontal",
            legend.direction = "horizontal",
            legend.margin = margin(1,1,1,1),
            legend.box.margin = margin(1,1,1,1),
            legend.title = element_text(size = 10, face = "bold",
                                        margin = margin(0,5,0,0)),
            legend.text= element_text(size = 10, margin = margin(0,7,0,0)),
            legend.justification = c(1, 0),
            legend.spacing.x = unit(1,'mm'),
            legend.key.size = unit(4, "mm"),
            legend.background = element_rect(fill = alpha("white", .8))
        ))
}


legend_below <- function() {
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.direction = "horizontal",
          legend.margin = margin(1,1,1,1),
          legend.box.margin = margin(1,1,1,1),
          legend.title = element_text(size = 10, face = "bold",
                                      margin = margin(0,5,0,0)),
          legend.text= element_text(size = 10, margin = margin(0,7,0,0)),
          legend.justification = c(1, 0),
          legend.spacing.x = unit(1,'mm'),
          legend.key.size = unit(4, "mm"),
          legend.background = element_rect(fill = alpha("white", .8)))
}

light_grid <- function() {
    theme(panel.grid.major.y = element_line(color = "gray90", linewidth =  .25),
          panel.grid.major.x = element_line(color = "gray90", linewidth = .25),
          panel.grid.minor.y = element_blank())
}


# Wrapper function to save ggplot plots
psave <- function(name = "", p = last_plot(),  width = 8.3,
                  height = 3.5, scale = 2,
                  device = cairo_pdf, units = "cm", output_dir) {
    name <- file.path(output_dir, name)
    message("Saving plot ", name)
    
    ggsave(name, plot = p, 
           width = width, height = height, scale = scale,
           device = device, units = units)
    
}



stability_colors <- c(mp(4, 7, 3, palette = palette_AE4))

names(stability_colors) <- c('unstable: ζ (-∞,-0.033)', 
                             'neutral: ζ [-0.033, 0.002)',
                             'stable: ζ [0.002, +∞)')


stability_color_scale <-  function() {
        scale_color_manual("Stability class", 
                           values = stability_colors)
}

stability_fill_scale <-  function() {
        scale_fill_manual("Stability class", 
                          values = stability_colors)
}


large_margins <- function() theme(plot.margin=grid::unit(c(10,10,10,10), "mm"))

no_guides <- function() {
    guides(fill = "none", linetype = "none", col = "none")
}


method_stats <- function(results_tbl, x, y, station_name,  method_name, scalar = "co2", 
                         metric = c("rel_bias", "uncert"), QC = FALSE) {
   metric <- match.arg(metric)
   text_color         <- mp(7, palette =  palette_AE4)
   er_slope <- results_tbl[station == station_name & method == method_name & 
                           scalar == scalar & QC == QC, first(Er_slope)]
   nondim_u <- results_tbl[station == station_name & method == method_name & 
                           scalar == scalar & QC == QC, first(nondim_u)]
   er_slope_txt <- ifelse(abs(er_slope*100) < 0.005, 
                          format(er_slope*100, scientific = TRUE, digits = 2),
                          round(er_slope*100, 2))
   message("er_slope_txt: ", er_slope_txt)
   nondim_u_txt <- ifelse(abs(nondim_u) < 0.005, 
                          format(nondim_u, scientific = TRUE, digits = 2),
                          round(nondim_u, 2))
   message("nondim_u_txt: ", nondim_u_txt)
   if (metric == "rel_bias") {
   geom_text(x = x, y = y, 
            label = paste0("Relative~bias~~",  er_slope_txt, "~symbol('%')"), 
              inherit.aes = FALSE, check_overlap = TRUE,
              hjust = 0,
              color = text_color,
              vjust = "left",
              family = "Noto sans", parse = TRUE, size = 3) 
   } else if (metric == "uncert") {
       geom_text(x = x, y = y, 
                label = paste0("Non-dim.~uncert.~~", nondim_u_txt), 
                  inherit.aes = FALSE, check_overlap = TRUE,
                  hjust = 0,
                  vjust = "left",
                  color = text_color,
                  family = "Noto sans", parse = TRUE, size = 3) 

   }
}


no_y_lab <- function() theme(axis.title.y = element_blank())


# Generic error plot for different methods
# Plots the error ~ flux value for a given method
err_plot <- function(method_name, scalar, station_name, 
                     results_tbl,
                     ylim = NULL, 
                     method_label_xy = NULL, 
                     error_label_xyy = NULL, 
                     QC = FALSE) {
    methods <- c("QEA", "REA-BTS", "REA-LDB-BW", "REA-DB1-BW", "REA-LDB-BCST", "REA-LDB-BTS")
    method_labels <- c("New~bold(QEA)~~method", 
                       "bold(REA)~~beta[Ts]~method", 
                       "bold(REA)~db[0.5]~beta[w]~method", 
                       "bold(REA)~db[1]~~beta[w]~method",
                       "bold(REA)~db[0.5]~~beta[ts-med]~method",
                       "bold(REA)~db[0.5]~beta[Ts]~method")
    error_var_names <- paste("fe", scalar, 
                             c("q", "bts", "ldb_bw", "db1_bw", "ldb_bcst", "ldb_bts"), 
                             sep = "_")
    flux_units_labs <- if (scalar == "co2") co2_flux_labs else h2o_flux_labs
    if (is.null(method_label_xy)) {
        method_label_xy <- if (scalar == "co2") c(-35, 9) else c(-1.5, 1.0)
    }
    if  (is.null(error_label_xyy)) {
        error_label_xyy <- if (scalar == "co2") c(-45, -8.5, -10) else c(-3, -1.0, -1.2)
    }
    if (is.null(ylim)) {
        ylim <- if (scalar == "co2") c(-9, 9) else c(-1.5, 1.5)
    }
    # Create the data frame
    method_data <- data.table(method = methods,
                              label = method_labels,
                              error_var_name = error_var_names, 
                              stringsAsFactors = FALSE)
    error_var_name <- method_data[method == method_name, error_var_name]
    ref_flux_name <- paste(scalar, "flux", sep = "_")
    message("Variable name: ", error_var_name)
    method_label <- method_data[method == method_name, label]
    message ("Method label: ", method_label)
    ggplot(perf_dt[station == station_name]) + 
        aes(.data[[ref_flux_name]], .data[[error_var_name]], shape = QC, col = zl_cat)+
        geom_hline(yintercept = 0, alpha = 0.3) + 
        geom_point(alpha = .8, size = point_size) +
        geom_smooth(method = "lm", se = FALSE, show.legend= FALSE, 
                    linewidth = 1, aes(group = NA), formula = y ~ x, 
                    col = rgb(1,.3,.0, alpha = .5)) +
        stability_color_scale() +
        coord_cartesian(ylim = ylim) +
        scale_shape_manual(name = "Quality",  values = c(3,16))+
        geom_text(x = method_label_xy[1], y = method_label_xy[2],
                  label = method_label, 
                  inherit.aes = FALSE, check_overlap = TRUE,
                  hjust = 0,
                  vjust = "left",
                  color = text_color,
                  family = "Noto sans", parse = TRUE, size = 3) +
        method_stats(results_tbl, method_name, "co2", station_name = station_name, 
                     metric = "rel_bias", x = error_label_xyy[1], 
                     y = error_label_xyy[2], QC = QC) +
        method_stats(results_tbl, method_name, "co2", station_name = station_name, 
                     metric = "uncert", x = error_label_xyy[1], 
                     y = error_label_xyy[3], QC = QC) +
        flux_units_labs() +
        theme_ae23() 
}

