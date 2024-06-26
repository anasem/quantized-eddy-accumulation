# Configuration for input data
{
    station_name        <- "Braunschweig"
    download_input_data <- TRUE
    subset              <- FALSE
    download_timeout    <- 1200 # [seconds] increase this if you have slow internet connection
    input_data_dir      <- file.path(project_dir, "input")
    raw_input_rds_fn    <- list.files(input_data_dir, full.names = TRUE, pattern = "rds")
}


{
    base_repo_url  <- "https://zenodo.org/records/11108797/files/"
    remote_files  <- 
        list(BS2020_06.rds = 
             list(url = paste0(base_repo_url, "BS2020_06.rds?download=1"),
                  md5 = "388da51534491bdd737442b5e4578998"),
             BS2020_10.rds =
                 list(url = paste0(base_repo_url, "BS2020_10.rds?download=1"),
                      md5 = "64de94bf7235a891e159b55bbc36c34b")
        )
    if (subset) {
        remote_files <- 
            list(BS2020_07_subset.rds = 
                 list(url = paste0(base_repo_url, "BS2020_07_subset.rds?download=1"),
                      md5 = "becab4b9f95f3bd02ccff5f9cbc1bc5d"))   
    }
}

# Paths for output directories and results files
{
    output_data_dir         <- file.path(project_dir, "output", station_name)
    results_data_dir        <- file.path(output_data_dir, "results")
    plot_output_dir         <- file.path(output_data_dir, "figures")
    sim_perf_eval_fn        <- file.path(results_data_dir, "perf_eval_sim.rds")
    
    perf_metrics_fn          <- file.path(results_data_dir, "perf_metrics_wd.rds") 
    perf_metrics_tbl_csv    <- file.path(results_data_dir, "performance_metrics.csv")
    performance_summary_csv <- file.path(results_data_dir, "performance_summary.csv") 
    
    sim_param_spc_fn        <- file.path(results_data_dir, "param_space_sim.rds")
    sim_errors_fn           <- file.path(results_data_dir, "sim_errors_summary.rds")
    nzero_w_err_fn          <- file.path(results_data_dir, "nonzero_w_errors.rds")
}

# Configuration for the simulation
{
    # Configuration for the performance evaluation simulation
    save_output               <- TRUE
    z_height                  <- 5
    dynamic_deadband_width    <- 0.5  # σw
    q0_quantization_threshold <- 0.4
    lookback_window           <- 300  # sec
    quantization_threshold    <- 2.4  # σw
    full_scale_value          <- 4.0  # σw
    atm_pressure              <- 1e5  # pascal
    directional_diffusion     <- TRUE
}

# Configuration for summarizing the simulation results
# Criteria for flagging the fluxes as good quality
{
    qc_Rs_wc_smaller_than     <- 0.3 # Steady state ratio
    qc_ustar_greater_than     <- 0.1 # Friction velocity
}

# Configuration for the parameter space simulation
{
    # Treatments
    # Adjust step_size to run fewer treatments (faster)
    step_size        <- 0.1
    qthreshold_vals  <- seq(0 ,  5, step_size)
    full_scale_vals  <- seq(.5,  6, step_size)
    number_of_cores  <- 48      # Number of cores you want to use
}


# Create folders if needed
if (!file.exists(results_data_dir)) {
    dir.create(results_data_dir, recursive = TRUE)
}

if (!file.exists(plot_output_dir)) {
    dir.create(plot_output_dir, recursive = TRUE)
}

