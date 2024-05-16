# This takes in perf_metrics_wd.rds and creates a tables of summarized results.
# ------------------------------------------------------------------------------

# Load sumamrized metrics
results_tbl <- readRDS(perf_metrics_fn)

# Transpose the results table to have methods as columns -----------------------

# Create the relative improvement table
unique_perm <- results_tbl[, .N,   by = c("scalar", "station", "QC")]

results_lst <- vector("list", unique_perm[, .N])
row_id <- 1
for (row_id in unique_perm[, .I]) {
    results_lst[[row_id]] <-
        transpose(results_tbl[station == unique_perm[row_id, station] & 
                  scalar == unique_perm[row_id, scalar] &
                  QC == unique_perm[row_id, QC],
                  !c("scalar", "station", "QC")], 
                  make.names = "method", keep.names = "Metric")

    results_lst[[row_id]][, scalar := unique_perm[row_id, scalar]]
    results_lst[[row_id]][, station := unique_perm[row_id, station]]
    results_lst[[row_id]][, QC := unique_perm[row_id, QC]]
}
results_tbl_t  <- rbindlist(results_lst, fill = TRUE)

# Improvements as ratios across all metrics 

cols <- c("QEA0", "REA-BTS", "REA-LDB-BTS", "REA-LDB-BCST", "REA-LDB-BW", 
          "REA-DB1-BTS", "REA-DB1-BCST", "REA-DB1-BW")

results_tbl_t[, (paste0(cols, "_rel")) := 
              lapply(.SD, function(x) round(abs(x/QEA), 2)), .SDcols = cols]

# ------------------------------------------------------------------------------
# We want to show the improvement across ALL scalars, by method

chosen_metrics <- c("Er_slope", "nondim_u", "Er_intercept", "Er_rmse")
results_tbl_rel_summary <- 
    results_tbl_t[Metric %in% chosen_metrics,.SD, by = .(Metric, scalar, QC), .SDcols = patterns("_rel")][, 
                  lapply(.SD, mean, na.rm = TRUE), by = .(Metric, QC), .SDcols = patterns("_rel")]

Quality_checked <-  TRUE
results_tbl_rel_summary_xtable <- 
    transpose(results_tbl_rel_summary[QC == Quality_checked,!"QC"], 
              make.names = "Metric", 
              keep.names = "Method")[order(Er_slope)]

# Creating a mapping data table with LaTeX syntax for readability
method_mapping <- data.table(
  ShortName = c("REA-DB1-BTS_rel", "REA-DB1-BCST_rel", "QEA0_rel", 
                "REA-DB1-BW_rel", 
                "REA-LDB-BCST_rel", "REA-BTS_rel", "REA-LDB-BTS_rel", 
                "REA-LDB-BW_rel"),
  LongName = c("$REA~(\\mathrm{db}~0.9\\sigma)~\\beta_{\\mathrm{ts}}$", 
               "$REA~(\\mathrm{db}~0.9\\sigma)~\\beta_{\\mathrm{ts-median}}$", 
               "$QEA0~\\text{without error diffusion}$", 
               "$REA~(\\mathrm{db}~0.9\\sigma)~\\beta_w$", 
               "$REA~(\\mathrm{db}~0.5\\sigma)~\\beta_{\\mathrm{ts-median}}$", 
               "$REA~\\beta_{ts}$", 
               "$REA~(\\mathrm{db}~0.5\\sigma)~\\beta_{ts}$", 
               "$REA~(\\mathrm{db}~0.5\\sigma)~\\beta_w$")
)


# Set key for joining on ShortName
setkey(results_tbl_rel_summary_xtable, Method)
setkey(method_mapping, ShortName)

# Join and update Method with LongName
results_tbl_rel_summary_xtable <- 
    results_tbl_rel_summary_xtable[method_mapping, roll = FALSE][
        , Method := ifelse(is.na(LongName), Method, LongName)]

results_tbl_rel_summary_xtable[, LongName := NULL]
setorder(results_tbl_rel_summary_xtable, Er_slope)

setnames(results_tbl_rel_summary_xtable, 
         c("Method", "Er_slope", "nondim_u", "Er_intercept", "Er_rmse"), 
         c("Method", "Error slope", "Nondim. uncertainty", "Error intercept", "RMSE"))

# print(xtable(results_tbl_rel_summary_xtable, digits = 0), type = "latex",
#       sanitize.text.function = identity, include.rownames = FALSE)

# ------------------------------------------------------------------------------

if (save_output) {
    fwrite(results_tbl_t, perf_metrics_tbl_csv)
    fwrite(results_tbl_rel_summary, performance_summary_csv)
}
