source("src/plots/deps.R")
source("src/config/default.R")

nzero_w_err_dt <- readRDS(nzero_w_err_fn)

ln_col <- unname(mp(8, palette = palette_AE4))

open_rates_plot <- 
    ggplot(nzero_w_err_dt[full_scale_value == 5 & directional_diffusion == TRUE]) + 
        geom_hline(yintercept = 2, linewidth = .5, alpha = .3) +
        geom_line(aes(qthreshold/full_scale_value, open_rate_up_hz, lty = "up"), 
                  col = ln_col)    +
        geom_line(aes(qthreshold/full_scale_value, open_rate_down_hz, 
                      lty = "down"), col = ln_col) +
        theme_ae23() + 
        legend_below() +
        labs (x = "Scaled quantization threshold", y = "Switching rate (Hz)") +
        coord_cartesian(xlim = c(0, .75)) +
        scale_color_discrete("Full scale value") +
        scale_linetype_discrete("reservoir")

# ------------------------------------------------------------------------------
source("src/simulation/load-simulation-data.R")

chosen_aiid <- 883897
#
dt <- ec_dt[aiid == chosen_aiid]
ec_dt[,.(aiid, time)]

c <- dt[aiid == chosen_aiid, CO2]
w <- dt[aiid == chosen_aiid, w]
n <- length(c)

w <- w- mean(w)


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

dt_sim[, id := seq_along(wq)]

rates <- dt_sim[, .(rate = sum(wq != 0)), by = .(ceiling(id/10), ratio)]

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
                          alpha = .6,
                          col = mp(7, palette = palette_AE4), 
                          # fill = mp(6, palette = palette_ae4),
                          show.legend = FALSE)  +
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
                     col = "#0000000f" ,
                     aes(x, y = as.factor(ratio), 
                         height = prob), size = .7) +
      theme_ae23()  +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90"),
            panel.grid.major.x = element_line(color = "gray95"),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank()) + 
      labs(x = "switching rate (hz)", y = "scaled quantization threshold")  +
      scale_fill_manual(values = c("gray90", "gray80", "gray70", "gray60")) 


psave(p = open_rates_plot | ridges_plot, name = "valve-opentimes-distribution.pdf", 
      width = 4.3 * 2, height = 4.3, scale = 2, 
      output_dir = plot_output_dir)


