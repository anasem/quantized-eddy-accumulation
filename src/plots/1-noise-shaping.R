# Plot 1-H(z); transfer function

source("src/plots/deps.R")
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
set.seed(22)
chosen_aiid <- ec_dt[, sample(aiid, 1)]
print(chosen_aiid)
#
dt <- ec_dt[aiid == chosen_aiid]
#
w <- dt[aiid == chosen_aiid, w]
N <- length(C)

qthreshold_ratios <- 0.5
high_level <- 4.5
spec_niter <- 8

base_spectrum <- as.data.table(psd::pspectrum(w,
                                              niter = spec_niter, 
                                              Nyquist.normalize = TRUE, 
                                              x.frqsamp = 10))
base_spectrum[, ratio:= "reference"]

wq_noed = quantize_w(w, sd(w)*0.6, full_scale_value = 1, 
                     damping_factor = 0)
noed_spectrum <- as.data.table(psd::pspectrum(wq_noed * sd(w)/ sd(wq_noed),
                                              niter = spec_niter, 
                                              Nyquist.normalize = TRUE, 
                                              x.frqsamp = 10))
noed_spectrum[, ratio:= "quantized"]


dt_sim <- 
    rbindlist(c(list(base_spectrum, noed_spectrum),
                lapply(qthreshold_ratios, w = w, 
           function(x, w) {
               wq = quantize_w(w, sd(w)*high_level*x, 
                               full_scale_value = sd(w)*high_level)
               w_psd <-  as.data.table(psd::pspectrum(wq,
                                niter = spec_niter, Nyquist.normalize = TRUE, 
                                x.frqsamp = 10))
               w_psd[, ratio:= x]
           })))

dt_sim[ratio == 0.5, ratio := "quantized + err. diffusion"]

spectra_plot <- 
    ggplot(dt_sim) + 
        aes(freq, spec, col = as.factor(ratio)) +
        geom_line(linewidth = 1) +
        scale_y_log10() + 
        theme_ae23() +
        scale_color_manual("", values = unname(mp(c(3,4,5), 
                           palette = palette_AE4)) ) +
        labs(x = "Frequency (Hz)", y = expression(S[ww])) + 
        scale_x_continuous(trans = log10_trans(), 
                           breaks = trans_breaks("log10", function(x) 10^x),
                            labels = trans_format("log10", math_format(10^.x))) + 
                            theme(legend.direction = "vertical", 
              legend.background = element_rect(fill = NA),
              legend.position = c(.1, .1),
              legend.justification = c(0,0),
              legend.box = "vertical")

p_spectra_and_noise_TF <- 
    spectra_plot/
        p_noise_shaping &
    plot_annotation(tag_levels = "a", tag_suffix = ")", tag_prefix = "(")  
    plot_layout(guides = "collect") 

psave(p = p_spectra_and_noise_TF, 
      name = "spectra-and-noise-tf.pdf", 
      width = 4.3*1.3, height = 4.3*1.6, scale = 2, 
      output_dir = plot_output_dir)

