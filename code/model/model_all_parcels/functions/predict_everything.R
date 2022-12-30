predict_everything <- function(.data, frr_id, buildings) {
  
  # Load model
  get_fit_path(frr_id, buildings) %>%
    
    readr::read_rds() %>%
    
    ### Use model fit to predict every obs in FRR
    predict(
      .data %>% select(!log_priceadj_ha)
    ) %>%
    
    ### Bind to original frr dataframe 
    bind_cols(.data)
}
