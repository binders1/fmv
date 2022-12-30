import_and_model <- function(frr_id, buildings) {
  # Load in data from current FRR
  data_to_model <- frr_import(frr_id, buildings, dir = "cleaned")
  
  # Fit FRR model
  model_fit <- model_everything(data_to_model, frr_id, buildings)
  
  data_to_predict <- frr_import(frr_id, buildings = TRUE, dir = "all_parcels")
  
  # Predict
  predict(model_fit, data_to_predict)
  
}

predict_and_save <- function(frr_id, buildings) {
  
  # Load in data from current FRR
  data_to_model <- frr_import(frr_id, buildings)
  
  # Predict all parcel values using fitted FRR model
  predict_everything(data_to_model, frr_id, buildings) %>%
    
    write_parquet(
      x = .,
      sink = get_pred_path(frr_id, buildings)
    )
  
}


get_fit_path <- function(frr_id, buildings) {
  
  # Save model object 
  fit_path <- 
    file.path(
      model_ddir, "predict_everything", "model_fits", 
      paste0("frr", frr_id)
    )
  
  if (buildings) {
    fit_path <-
      fit_path %>% 
      paste0("_bldg_all_fit.rds")
  } else {
    fit_path <-
      fit_path %>% 
      paste0("_nobldg_all_fit.rds")
  }
  
  fit_path
}


get_pred_path <- function(frr_id, buildings) {
  
  pred_path <- 
    file.path(model_ddir, "predict_everything", 
              "model_predictions",
              paste0("all_predicted_frr_", frr_id))
  
  if (buildings) {
    pred_path <-
      prediction_file %>%
      paste0("_bldg.pqt")
  } else {
    pred_path <- 
      prediction_file %>%
      paste0("_no_bldg.pqt")
  }
  
  pred_path

}







