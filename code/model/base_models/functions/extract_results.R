
# Generic result-extracting function that dispatches to county and frr methods
extract_results <- function(geo = c("county", "frr"), ...) {
  geo <- match.arg(geo)
  extract_fn <- switch(geo,
                       county = extract_results.county,
                       frr = extract_results.frr)
  extract_fn(...)
}

# FRR method ==================================================================
extract_results.frr <- function(rf_results_list, frr) {
  
  # Extract ALL predictions
  frr_predict_all <-
    rf_results_list$predict_all
  
  # Extract test predictions
  frr_predictions <-
    #' @note now use augment to avoid weird row issue 
    augment(rf_results_list$rf_last_fit) %>%
    #collect_predictions(rf_results_list$rf_last_fit) %>%
    select(sid, .pred, log_priceadj_ha)

  # Extract variable importance
  frr_importance <- 
    rf_results_list$rf_last_fit$.workflow[[1]] %>%
    extract_fit_parsnip() %>%
    vip::vi() %>%
    pivot_wider(
      names_from = Variable,
      values_from = Importance
    ) %>%
    mutate(frr = frr) %>%
    relocate(frr)
  
  # Extract performance metrics 
  frr_metrics <-
    collect_metrics(rf_results_list$rf_last_fit) %>%
    select(-c(.estimator, .config)) %>%
    pivot_wider(
      names_from = .metric,
      values_from = .estimate
    ) %>%
    mutate(mse = rmse^2, .keep = "unused") %>%
    bind_cols(rf_results_list$frr_stats) %>%
    relocate(frr)
  
  list(
    "predict_all" = frr_predict_all,
    "predictions" = frr_predictions,
    "importance" = frr_importance,
    "metrics" = frr_metrics
  )
  
}

# County method ===============================================================
extract_results.county <- function(rf_results_list) {
  
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

# Helper functions ============================================================
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
