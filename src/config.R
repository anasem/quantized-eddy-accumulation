# Configuration for input data
{
    download_input_data <- TRUE
    download_timeout <- 600 # [seconds] increase this if you have slow internet connection
    raw_input_rds_fn <- file.path(project_dir, "input/raw_data_input.rds")
}

# Paths for output directories and results files
{
    output_data_dir        <- file.path(project_dir, "output/results")
    plot_output_dir        <- file.path(project_dir, "output/figures")
    sim_perf_eval_fn       <- file.path(output_data_dir, "sim_perf_eval.rds")
    sim_param_spc_fn       <- file.path(output_data_dir, "sim_param_spc.rds")
    results_tbl_fn         <- file.path(output_data_dir, "results_tbl.rds")
    sim_errors_fn          <- file.path(output_data_dir, "sim_errors_summary.rds")
    nzero_w_err_fn         <- file.path(output_data_dir, "nonzero_w_errors.rds")
    results_xtable_fn      <- file.path(output_data_dir, "perf_eval_table1.csv")
    results_tbl_summary_fn <- file.path(output_data_dir, "perf_eval_imrovement_summary.csv")
    results_tbl_t_fn       <- file.path(output_data_dir, "perf_eval_all.csv")
}

# Configuration for the simulation
{
    # Configuration for the performance evaluation simulation
    save_output               <- TRUE
    z_height                  <- 5
    dynamic_deadband_width    <- 0.5  # σw
    q0_quantization_threshold <- 0.4
    lookback_window           <- 300  # sec
    quantization_threshold    <- 2    # σw
    full_scale_value          <- 4.0  # σw
    atm_pressure              <- 1e5  # pascal
    directional_diffusion     <- TRUE
    station_name              <- "BS"
}


# Configuration for the parameter space simulation
{
    # Treatments
    # Adjust step_size to run fewer treatments (faster)
    step_size <- 0.1
    qthreshold_vals <- seq(0 ,  5, step_size)
    full_scale_vals <- seq(.5,  6, step_size)
    number_of_cores <- 12      # Number of cores you want to use
}


