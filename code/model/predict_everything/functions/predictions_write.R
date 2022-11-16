predictions_write <- function(frr_id, buildings) {
  
  if (buildings) {
    prediction_file <- 
      file.path(model_ddir, "predict_everything",
                paste0("all_predicted_frr_", frr_id, "_bldg.pqt"))
  } else {
    prediction_file <- 
      file.path(model_ddir, "predict_everything",
                paste0("all_predicted_frr_", frr_id, "_no_bldg.pqt"))
    
  }
  
  frr_import(frr_id, buildings) %>%
    
    predict_everything() %>%
    
    write_parquet(
      prediction_file
    )
  
}