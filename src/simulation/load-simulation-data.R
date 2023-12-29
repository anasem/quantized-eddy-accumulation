message("Loading input data")

# Check if the input data file exists
if (!file.exists(raw_input_rds_fn)) {
    stop(paste0("Input data file '", raw_input_rds_fn, "' does not exist. Please download it."))
}

if (!exists("ec_dt")) {
    ec_dt <- readRDS(raw_input_rds_fn)
}

message("Input data loaded")
ec_dt[, is_updraft := w > 0]
ec_dt[, aiid := floor(as.numeric(time)/30/60)]
ec_dt[, N_tot := .N, by = aiid]
