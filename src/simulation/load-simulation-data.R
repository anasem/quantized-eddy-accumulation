message("Loading input data")

# Subset existing files to use only the ones that are needed
if (download_input_data) {
    raw_input_rds_fn  <- raw_input_rds_fn[basename(raw_input_rds_fn) %in% names(remote_files)]
}

# Check if the input data file exists
if (!all(sapply(raw_input_rds_fn, file.exists))) {
    stop(paste0("Input data file '", raw_input_rds_fn, "' does not exist. Please download it."))
}



ec_dt <- rbindlist(lapply(raw_input_rds_fn, readRDS))

message("Input data loaded ")
ec_dt[, is_updraft := w > 0]
ec_dt[, aiid := floor(as.numeric(time)/30/60)]
ec_dt[, N_tot := .N, by = aiid]
