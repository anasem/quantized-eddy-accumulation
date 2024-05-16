# Configuration for merged dataset from Hainich and Braunschweig

# Configuration for input data
{
    station_name               <- "Hai and BS"
}

# Paths for output directories and results files
{
    output_data_dir        <- file.path(project_dir, "output", station_name)
    results_data_dir       <- file.path(output_data_dir, "results")
    plot_output_dir        <- file.path(output_data_dir, "figures")
    sim_perf_eval_fn       <- file.path(results_data_dir, "perf_eval_sim.rds")
    
    perf_metrics_fn         <- file.path(results_data_dir, "perf_metrics_wd.rds") 
    perf_metrics_tbl_csv      <- file.path(results_data_dir, "performance_metrics.csv")
    performance_summary_csv <- file.path(results_data_dir, "performance_summary.csv") 
    
    sim_param_spc_fn       <- file.path(results_data_dir, "param_space_sim.rds")
    sim_errors_fn          <- file.path(results_data_dir, "sim_errors_summary.rds")
    nzero_w_err_fn         <- file.path(results_data_dir, "nonzero_w_errors.rds")
}


# Create folders if needed
if (!file.exists(results_data_dir)) {
    dir.create(results_data_dir, recursive = TRUE)
}

if (!file.exists(plot_output_dir)) {
    dir.create(plot_output_dir, recursive = TRUE)
}

