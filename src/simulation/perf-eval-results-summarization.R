# Read simulation reults -------------------------------------------------------

message("Reading results files")

perf_eval  <- rbindlist(lapply(sim_perf_eval_fn, readRDS))


message("Summarizing errors")
# Error summary for method perforamnce evaluation

summarize_perf_eval <-  function(perf_eval)  {
    results_tbl_err <- 
        rbind(
              perf_eval[, c(error_summary(fe_co2_q, co2_flux), 
                            method = "QEA", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_co2_q0, co2_flux), 
                            method = "QEA0", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_co2_bts, co2_flux), 
                            method = "REA-BTS", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_co2_ldb_bts, co2_flux), 
                            method = "REA-LDB-BTS", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_co2_ldb_bcst, co2_flux), 
                            method = "REA-LDB-BCST", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_co2_ldb_bw, co2_flux), 
                            method = "REA-LDB-BW", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_h2o_q, h2o_flux), 
                            method = "QEA", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_h2o_q0, h2o_flux), 
                            method = "QEA0", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_h2o_bts, h2o_flux), 
                            method = "REA-BTS", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_h2o_ldb_bts, h2o_flux), 
                            method = "REA-LDB-BTS", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_h2o_ldb_bcst, h2o_flux), 
                            method = "REA-LDB-BCST", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_h2o_ldb_bw, h2o_flux), 
                            method = "REA-LDB-BW", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_ts_q, ts_flux), 
                            method = "QEA", scalar = "ts"), by = station],
              perf_eval[, c(error_summary(fe_ts_ldb_bw, ts_flux), 
                            method = "REA-LDB-BW", scalar = "ts"), by = station],
              # DB1 treatments
              perf_eval[, c(error_summary(fe_co2_db1_bts, co2_flux), 
                            method = "REA-DB1-BTS", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_co2_db1_bcst, co2_flux), 
                            method = "REA-DB1-BCST", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_co2_db1_bw, co2_flux), 
                            method = "REA-DB1-BW", scalar = "co2"), by = station],
              perf_eval[, c(error_summary(fe_h2o_db1_bts, h2o_flux), 
                            method = "REA-DB1-BTS", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_h2o_db1_bcst, h2o_flux), 
                            method = "REA-DB1-BCST", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_h2o_db1_bw, h2o_flux), 
                            method = "REA-DB1-BW", scalar = "h2o"), by = station],
              perf_eval[, c(error_summary(fe_ts_db1_bw, ts_flux), 
                            method = "REA-DB1-BW", scalar = "ts"), by = station]
        )

    results_nondim_err <- 
        rbind(
              perf_eval[, c(nondim_err_stats(ndfe_co2_q), 
                            method = "QEA", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_co2_q0), 
                            method = "QEA0", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_co2_bts), 
                            method = "REA-BTS", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_co2_ldb_bts), 
                            method = "REA-LDB-BTS", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_co2_ldb_bcst), 
                            method = "REA-LDB-BCST", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_co2_ldb_bw), 
                            method = "REA-LDB-BW", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_q), 
                            method = "QEA", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_q0), 
                            method = "QEA0", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_bts), 
                            method = "REA-BTS", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_ldb_bts), 
                            method = "REA-LDB-BTS", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_ldb_bcst), 
                            method = "REA-LDB-BCST", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_ldb_bw), 
                            method = "REA-LDB-BW", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_Ts_q), 
                            method = "QEA", scalar = "ts"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_ts_ldb_bw), 
                            method = "REA-LDB-BW", scalar = "ts"), by = station],
              # DB1 treatments
              perf_eval[, c(nondim_err_stats(ndfe_co2_db1_bts), 
                            method = "REA-DB1-BTS", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_co2_db1_bcst), 
                            method = "REA-DB1-BCST", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_co2_db1_bw), 
                            method = "REA-DB1-BW", scalar = "co2"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_db1_bts), 
                            method = "REA-DB1-BTS", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_db1_bcst), 
                            method = "REA-DB1-BCST", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_h2o_db1_bw), 
                            method = "REA-DB1-BW", scalar = "h2o"), by = station],
              perf_eval[, c(nondim_err_stats(ndfe_ts_db1_bw), 
                            method = "REA-DB1-BW", scalar = "ts"), by = station]
        )


    results_tbl <- 
        merge(results_tbl_err, results_nondim_err, 
              by = c("station", "method", "scalar"))

    return(results_tbl)
}


results_tbl    <- summarize_perf_eval(perf_eval)
results_tbl[, QC := FALSE]
results_tbl_QC <- summarize_perf_eval(perf_eval[QC == "OK"])
results_tbl_QC[, QC := TRUE]

if (save_output) {
    message("Saving summarized simultion results")
    # Add suffix and extension to the summary file
    saveRDS(rbind(results_tbl, results_tbl_QC), perf_metrics_fn)
}

