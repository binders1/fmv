

predict_all_parcels <- function(frr_id) {
  # Load in data from current FRR
  data_to_model <- 
    frr_import(frr_id, buildings = FALSE, dir = "cleaned")
  
  # Load in data of all parcels
  data_to_predict <- 
    frr_import(frr_id, buildings = TRUE, dir = "all_parcels") %>%
    rename(MedianHomeValue = "median_listing_price")
  
  # Subset modeling data to only variables that 
  # appear in both datasets, + price and sid (id var for model)
  vars_to_model <- 
    base::intersect(
      names(data_to_model),
      names(data_to_predict)
    ) %>%
    c("log_priceadj_ha", "sid")
  
  data_to_model_subset <- 
    data_to_model[vars_to_model]
    
  # Fit FRR model
  model_fit <- 
    model_everything(
      data_to_model_subset, 
      frr_id, buildings = FALSE
      )
  
  # Predict predictions 
  predicted_price <-
    predict(model_fit, data_to_predict) 
  
  # Append to base prediction dataset
  augment_pred <- 
    bind_cols(predicted_price, data_to_predict)
    
  # write to memory
  file_path <- 
    file.path(
      ddir, "model", "model_all_parcels",
      paste0("model_all_parcels_frr", frr_id, ".pqt")
    )
  
  write_parquet(augment_pred, file_path)
  
  message("Saved: ", file_path)
}





