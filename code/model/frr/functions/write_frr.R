
# write_frr_predictions

write_frr <- function() {
  
  
  ## Write stats ####
  
  ffb_dir <- file.path(ddir, "model/full/frr/base")
  
  pred_dir <- file.path(ffb_dir, "predictions")
  perform_dir <- file.path(ffb_dir, "performance")
  imp_dir <- file.path(ffb_dir, "importance")
  
  pred_file <- 
    file.path(
      pred_dir, paste0("pred_ffb_", k, ".pqt")
    )
  
  perform_file <-  
    file.path(
      perform_dir, paste0("stats_ffb_", k, ".pqt")
    )
  
  imp_file <-  
    file.path(
      imp_dir, paste0("import_ffb_", k, ".pqt")
    )
  
  write_parquet(frr_predictions,
                pred_file
                )
  
  write_parquet(frr_stats,
                perform_file
                )
  
  write_parquet(frr_importance,
                imp_file
                )
  
  
}