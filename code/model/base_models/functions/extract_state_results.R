
extract_state_results <- function(rf_results_list) {
  
  null_counties <- map_lgl(rf_results_list, is.null)
  
  non_null_rf <- rf_results_list[!null_counties]
  
  state_model_summary <-
    map_dfc(
      .x = c("county_stats", "rf_last_fit"),
      .f = ~ rf_pluck(list = non_null_rf, element = .x)
    ) %>%
    select(!c(splits, id, .notes))
  
  state_predict_all <- rf_pluck(non_null_rf, "predict_all")
  
  state_predictions <- 
    bind_rows(state_model_summary$.predictions)
  
  state_importance <-
    map2_dfr(
      .x = state_model_summary$.workflow,
      .y = state_model_summary$fips,
      .f = extract_var_importance
    )
  
  state_metrics <-
    state_model_summary %>%
    select(-c(.predictions, .workflow)) %>%
    extract_metrics()
    
  list(
    "predict_all" = state_predict_all,
    "predictions" = state_predictions,
    "importance" = state_importance,
    "metrics" = state_metrics
  )
  
}

rf_pluck <- function(list, element) {
  map_dfr(
    list,
    ~ pluck(.x, element)
  )
}

extract_var_importance <- function(workflow, fips) {
  
  workflow %>%
    extract_fit_parsnip() %>%
    vip::vi() %>%
    pivot_wider(
        names_from = Variable,
        values_from = Importance
    ) %>%
    mutate(fips = fips) %>% 
    relocate(fips)
}

extract_metrics <- function(data) {
  
  data %>%
    unnest(.metrics) %>%
    select(-c(.estimator, .config)) %>%
    pivot_wider(
      names_from = .metric,
      values_from = .estimate
    ) %>%
    mutate(mse = rmse^2, .keep = "unused")

}
