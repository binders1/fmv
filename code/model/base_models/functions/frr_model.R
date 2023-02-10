
frr_model <- function(frr, pred.set, only.nolte.counties) {
  
  model_predictors <-
    predictor_set(geo = "frr", pred.set = pred.set)
  
  frr_data <-
    read_frr_data(
      frr = frr, only.nolte.counties = only.nolte.counties
    ) %>%
    select(any_of(model_predictors)) %>%
    mutate(state = str_sub(fips, 1, 2))
  
  model_data <-
    frr_data %>%
    frr_feature_selection(model_predictors = model_predictors)
  
  frr_rf_fit <-
    rf_fit(geo = "frr", frr, model_data)

  frr_results <- 
    extract_results(geo = "frr", frr_rf_fit, frr) %>%
    sort_list()
  
  result_paths <-
    make_results_path(
      geo = "frr", 
      frr = frr,
      model_abbr = make_model_abbr("frr", pred.set, only.nolte.counties)
      ) %>%
    sort_list()
     
  walk2(
    .x = frr_results,
    .y = result_paths,
    .f = ~write_parquet_verbose(.x, .y)
    )
  }
