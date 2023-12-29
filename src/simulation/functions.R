
# Calculate biased covariance
bcov <- function (x,y) {
    N <- length(x)
    cov(x,y, use = "na.or.complete")*(N-1)/N
}


# Analytical value of Beta when C and w are from a joint gaussian
# Copied form helper function in nonzero-w-paper repo
beta_analytic <- function(w_mean, w_sd) {
    -sqrt(pi) * exp(w_mean ^ 2 / w_sd ^ 2 / 2) * sqrt(2) * 
        (erf(w_mean * sqrt(2) / w_sd / 2) ^ 2 - 1) / 4;
}



# Monin-Obukhov length L is a length scale (m) proportional to the height above
# surface where bouyant turbulence first dominates shear production of
# turbulence.
# Tp potential temperature
# w_Ts covariance between w and sonic temperature
# k = 0.41 Von Karman constant
Obukhov_length <- function(u_star, w_Ts, Tp) {
    k <- 0.41
    g <- 9.81 
    L <- -(Tp*u_star^3)/
          (k*g*w_Ts)
    return(L)
}

# Calculate Obukov length from uvw
Obukhov_length_uvw <- function(u, v,w, Ts, P) {
    ustar <- friction_velocity_uvw(u,v,w)
    Tp <- potential_temperature(mean(Ts, na.rm = TRUE), P = P)
    w_Ts <- bcov(w, Ts)
    Obukhov_length(ustar, w_Ts, Tp)
}


#' Calcualte friction velocity [m s-1]
#'
#' Friction velocity (u_star) can be expressed using kinematic momentum 
#' fluxes in the x and y directions
#' @param u_w covariance between u and w wind componenets
#' @param v_w covariance between v and w wind componenets
friction_velocity <- function(u_w, v_w) {
    u_star <- (u_w^2 + v_w^2)^.25
    return(u_star)
}


friction_velocity_uvw <- function(u,v,w) {
    u_w <- bcov(u,w)
    v_w <- bcov(v,w)
    ustar <- friction_velocity(u_w, v_w)

    return(ustar)
}

#' Potential temperature according to Poisson's equation
#'
#' @param Ta temperature or air parcel
#' @param p Pressure [Pa] of air parcel
potential_temperature <- function(Ta, P) {
    T_p <- Ta * (1e5/P)^.286
    return(T_p)

}

#' Calculate wind speed
#'  
#' Calculate wind speed from zonal and meridional wind components
#' 
#' @param u wind velocity component 
#' @param v wind velocity component
#' 
#' @return: numeric vector of the same length and units as u and v.
wind_speed <- function(u,v) {
  stopifnot(length(u) == length(v))
  return (sqrt(u^2+v^2))
}


#' Calculate relaxed eddy accumulation beta
#' 
#'  Beta = F / (ΔC * σw)
rea_beta <- function(w, C, deadband = 0) {
    delta_c <- mean(C[w>deadband], na.rm = TRUE) - mean(C[w < -deadband], 
                                                        na.rm = TRUE)
    beta = bcov(w, C)/(sd(w [abs(w) > deadband], na.rm = TRUE) * delta_c)
    return(beta)
}

#' Calculate REA beta based on  ΔC and σw
#'
rea_beta2 <- function(flux, sd_w, delta_c) {
    flux/(sd_w * delta_c)
}


# β as σw/Δ<w>, from (Baker, 2000)
# See also (Vogl et al. 2021 - Eq. 7)
rea_beta_w <- function(w) {
    delta_w <- mean(w[w>0], na.rm = TRUE) - mean(w[w < 0], na.rm = TRUE)
    sd(w, na.rm = TRUE) / delta_w
}


rea_beta_w2  <- function(delta_w, w_sd){
    w_sd/delta_w
}


# Calculate alpha(c,w) = <c'|w'|>/<c'w'>
alpha_xy <- function(C,w) {
    bcov(C,abs(w))/ bcov(C,w)
}


# Summarize an error calculated as error := true - measured
# Reference is the "true" value of the measured variable
error_summary <- function(error, reference) {
    lm_coefs <- summary(lm(error ~ reference))$coefficients
    lm1_coefs <- summary(lm(error ~ reference - 1))$coefficients
    list (Er_mean = mean(error, na.rm = TRUE), 
          Er_mean_rel = mean(error, na.rm = TRUE)/mean(reference, na.rm = TRUE),
          flux_mean = mean(reference, na.rm = TRUE),
          flux_sum = sum(reference, na.rm = TRUE),
          flux_sd = sd(reference, na.rm = TRUE),
          Er_sum = sum(error, na.rm = TRUE),
          MAE = mean(abs(error), na.rm = TRUE),
          MAE_rel = mean(abs(error)/abs(reference), na.rm = TRUE),
          Er_sd = sd(error, na.rm = TRUE), 
          Er_rmse = sqrt(mean(error^2, na.rm = TRUE)),
          Er_intercept = lm_coefs[1,1],
          Median_er_ratio = median((reference - error)/reference, na.rm = TRUE),
          Er_slope = lm_coefs[2,1],
          Er_slope1 = lm_coefs[1,1],
          Er_slope1.p = lm_coefs[1,4],
          Er_intercept.p = lm_coefs[1,4],
          Er_slope.p = lm_coefs[2,4],
          Er_intercept.se = lm_coefs[1,2],
          Er_slope.se = lm_coefs[2,2]
    )
}



# Cut stability into three classes
cut_zL3 <- function(zL) {
    zL_labels = c('unstable: ζ (-∞,-0.033)', 
                  'neutral: ζ [-0.033, 0.002)',
                  'stable: ζ [0.002, +∞)')
    cut(zL, breaks = c(-Inf, -.033, 0.002, Inf), right = FALSE, zL_labels)
}

