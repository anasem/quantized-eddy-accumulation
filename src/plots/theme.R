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
mp <- function(..., palette = pallete_AE1) {
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
        return(main_palette[unlist(chosen_colors)])
    }
}

geom_1to1  <- function(intercept = 0, ...) {
    geom_abline(slope = 1, intercept = intercept, ...)
}

# My theme
theme_ae23 <- function() {
  return (theme_light() + 
    theme(text = element_text(family="Source Sans Pro", size = 10),
            plot.subtitle = element_text(color = "gray50"),
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(color = "gray50"),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.text.x= element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.margin=grid::unit(c(1,4,1,1), "mm"),
            legend.position = c(1, 0),
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
            legend.background = element_rect(fill = alpha("white", .8)),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank()
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



stability_colors <- c(mp(4, 7, 2, palette = palette_AE4))

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


method_stats <- function(results_tbl, x, y, method_name, scalar = "co2", 
                         metric = c("slope", "mean_bias", "rmse") ) {
   metric <- match.arg(metric)
   text_color         <- mp(7, palette =  palette_AE4)
   mean_err <- results_tbl[method == method_name & scalar == scalar, first(Er_mean_rel)]
   er_slope <- results_tbl[method == method_name & scalar == scalar, first(Er_slope)]
   er_sd <- results_tbl[method == method_name & scalar == scalar, first(Er_sd)]
   er_slope_txt <- ifelse(er_slope < 0.005, 
                          format(er_slope, scientific = TRUE, digits = 2),
                          round(er_slope, 2))
   if (metric == "mean_bias") {
   geom_text(x = x, y = y, 
            label = paste0("bias~~",  round(mean_err*100, 2), "~symbol('%')"), 
              inherit.aes = FALSE, check_overlap = TRUE,
              hjust = 0,
              color = text_color,
              vjust = "left",
              family = "Noto sans", parse = TRUE, size = 3) 
   } else if (metric == "slope") {
       geom_text(x = x, y = y, 
                label = paste0("slope~~", er_slope_txt), 
                  inherit.aes = FALSE, check_overlap = TRUE,
                  hjust = 0,
                  vjust = "left",
                  color = text_color,
                  family = "Noto sans", parse = TRUE, size = 3) 

   } else if (metric == "rmse") {
       geom_text(x = x, y = y, 
                label = paste0("RMSE~~", round(er_sd, 1)), 
                  inherit.aes = FALSE, check_overlap = TRUE,
                  hjust = 0,
                  vjust = "left",
                  color = text_color,
                  family = "Noto sans", parse = TRUE, size = 3) 

   }
}
