source("src/plots/deps.R")
source("src/simulation/load-simulation-data.R")

nzero_w_err_dt <- readRDS(nzero_w_err_fn)

ln_col <- unname(mp(8, palette = palette_AE4))

open_rates_plot <- 
    ggplot(nzero_w_err_dt[full_scale_value %in% c(5) & directional_diffusion == TRUE & station == "BS"]) + 
        geom_hline(yintercept = 2, linewidth = .5, alpha = .3) +
        geom_line(aes(qthreshold/full_scale_value, open_rate_up_hz, lty = "up"), col = ln_col)    +
        geom_line(aes(qthreshold/full_scale_value, open_rate_down_hz, 
                      lty = "down"), col = ln_col) +
        theme_ae23() + 
        legend_below() +
        labs (x = "Scaled quantization threshold", y = "Open rate (Hz)") +
        coord_cartesian(xlim = c(0, .75)) +
        scale_color_discrete("Full scale value") +
        scale_linetype_discrete("reservoir")

# ------------------------------------------------------------------------------
chosen_aiid <- 886146
#
dt <- ec_dt[aiid == chosen_aiid]

C <- dt[aiid == chosen_aiid, CO2]
w <- dt[aiid == chosen_aiid, w]
N <- length(C)

if (TRUE) {
    w <- w- mean(w)
}


# ------------------------------------------------------------------------------

qthreshold_ratios <- c(0.01, 0.2, 0.3, 0.5)
full_scale_value <- 5

dt_sim <- 
    rbindlist(
    lapply(qthreshold_ratios, w = w, 
           function(x, w) {
               data.table(ratio = x, 
                          wq = quantize_w(w, sd(w)*4*x, 
                                          full_scale_value = sd(w)*4))
           }))

dt_sim[, ID := seq_along(wq)]

rates <- dt_sim[, .(rate = sum(wq != 0)), by = .(ceiling(ID/10), ratio)]

theoretical_rates  <- rates[, .(rate = mean(rate)), by = ratio]
theoretical_rates <- 
    theoretical_rates[, .(x = seq(0, 10), 
                      prob = dbinom(seq(0, 10), size = 10, p = rate/10)), 
                      by = .(ratio, rate)]

ridges_plot <- 
    ggplot(rates, aes(x = rate, y = as.factor(ratio))) +
      geom_density_ridges(scale = 3,
                          aes(fill = as.factor(ratio), 
                              height = after_stat(density)), 
                          size = .3, stat = 'binline',
                          draw_baseline = TRUE, 
                          binwidth = 1,
                          alpha = .5,
                          color = "gray30", show.legend = FALSE)  +
      geom_ridgeline(data = theoretical_rates, 
                     scale = 3,
                     fill = NA, show.legend = FALSE, lty = 3,
                     aes(x, y = as.factor(ratio), 
                         height = prob), 
                     col = unname(mp(2, palette = palette_AE4)),
                     size = .5) +
      geom_ridgeline(data = theoretical_rates, 
                     scale = 3,
                     fill = NA, show.legend = FALSE, lty = 1,
                     col = "#0000000F" ,
                     aes(x, y = as.factor(ratio), 
                         height = prob), size = .7) +
      theme_ae23()  +
      scale_fill_manual(values = unname(mp(1:4 + 2, palette = pallete_AE3))) +
      theme(text = element_text(family = "Carrois Gothic"),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90"),
            panel.grid.major.x = element_line(color = "gray95"),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            plot.subtitle = element_text(color = "gray50"),
            plot.caption = element_text(color = "gray50"),
            plot.title = element_text(size = 18),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      labs(x = "Open rate (Hz)", y = "Scaled quantization threshold") 
#

psave(p = open_rates_plot | ridges_plot, name = "valve-opentimes-distribution.pdf", 
      width = 4.3 * 2, height = 4.3, scale = 2, 
      output_dir = plot_output_dir)


