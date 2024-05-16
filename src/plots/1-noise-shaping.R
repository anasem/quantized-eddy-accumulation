# Plot 1-H(z); transfer function

source("src/plots/deps.R")
source("src/config/default.R")
source("src/simulation/load-simulation-data.R")


# We construct the filter H[z] = 1/z
N      <- 18000
omega  <- seq(0, 2*pi, length.out = N)
z      <- exp(1i*omega*1)
Hz     <- 1-1/z

dt <- data.table(freq = seq(1/N, 5, length.out = 1000))
dt[, z := exp(1i*2*pi*freq/10)]
dt[, Si := 1-1/z]
dt[, S := abs(Si)]
dt[, S_theta := angle(Si)]

p_noise_shaping <- 
    ggplot(dt) + 
        geom_line(aes(freq, S), linewidth = 1, 
                  color = unname(mp(7, palette = palette_AE4))) +
        theme_ae23() +
        scale_x_continuous(trans = log10_trans(), 
                           breaks = trans_breaks("log10", function(x) 10^x),
                            labels = trans_format("log10", math_format(10^.x))) +
        labs(x = "Frequency (Hz)", y = "Magnitude")  +
        light_grid()


# Plot spectra of quantized signal vs. true signal. ----------------------------

N <- length(C)

qthreshold_ratio <- 0.6
high_level       <- 4.5
spec_niter       <- 6

# Randomly select 50 aiids with correlation > 0.3 and sd(w) > 0.4
set.seed(123)
selected_aiids <- ec_dt[, .(r =  cor(w, CO2), w_sd = sd(w)), 
                 by = aiid][abs(r) > 0.3 & w_sd > 0.4, sample(aiid, 50)]

ec_dt[, wq := quantize_w(w, sd(w)*high_level*qthreshold_ratio, 
                         full_scale_value = sd(w)*high_level), by = aiid]
ec_dt[, wq_noed := quantize_w(w, sd(w)*0.6, full_scale_value = 1, damping_factor = 0)]
ec_dt[, wq_noed := wq_noed * sd(w)/ sd(wq_noed)]

dt_sim <- 
    rbindlist(lapply(selected_aiids,  
                     function(x, ec_dt) {
                         message("Processing aiid: ", x)
                         w   <-  ec_dt[aiid == x, w]
                         wq  <-  ec_dt[aiid == x, wq]
                         wq_noed <-  ec_dt[aiid == x, wq_noed]
                         w_psd <-  as.data.table(psd::pspectrum(w,
                                niter = spec_niter, Nyquist.normalize = TRUE, 
                                x.frqsamp = 10))
                         wq_psd <-  as.data.table(psd::pspectrum(wq,
                                niter = spec_niter, Nyquist.normalize = TRUE, 
                                x.frqsamp = 10))
                         wq_noed_psd <-  as.data.table(psd::pspectrum(wq_noed,
                                niter = spec_niter, Nyquist.normalize = TRUE, 
                                x.frqsamp = 10))
                         w_psd[, aiid := x]
                         wq_noed_psd[, aiid := x]
                         wq_noed_psd[, signal := "quantized"]
                         w_psd[, signal := "original"]
                         wq_psd[, aiid := x]
                         wq_psd[, signal := "quantized + error diffusion"]
                         rbindlist(list(w_psd, wq_psd, wq_noed_psd))
           }, ec_dt = ec_dt))

dt_sim_summary <- dt_sim[, .(spec = mean(spec)), by = .(freq, signal)]

spectra_plot <-
    ggplot(dt_sim[]) + 
        aes(freq, spec, col = as.factor(signal), group = interaction(aiid, signal)) +
        # geom_line(linewidth = 0.1, alpha = .3) + # Faint lines for each aiid to
        # show the variability of the spectra.
        scale_y_log10() + 
        geom_line(data = dt_sim_summary, 
                  aes(freq, spec, col = as.factor(signal), group = signal), 
                  linewidth = 1.5) +
        theme_ae23() +
        scale_color_manual("", values = mp(c(7,4,3), palette = palette_AE4) ) +
        labs(x = "Frequency (Hz)", y = expression(S[ww])) + 
        scale_x_continuous(trans = log10_trans(), 
                           breaks = trans_breaks("log10", function(x) 10^x),
                            labels = trans_format("log10", math_format(10^.x))) + 
                            theme(legend.direction = "vertical", 
              legend.background = element_rect(fill = NA),
              legend.position.inside = c(.1, .1),
              legend.justification = c(0,0),
              legend.box = "vertical")

p_spectra_and_noise_TF <- 
    spectra_plot + p_noise_shaping 

ggsave(plot = p_spectra_and_noise_TF,
      filename = file.path(plot_output_dir, "noise-shaping.pdf"), device = cairo_pdf,
      width = 4.3*2.5, height = 4.3*1.1, scale = 1.8, units = "cm")

