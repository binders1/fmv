# frr_model() =================================================================
# 
# Runs model for a given FRR
# 
## frr: character, FRR name as it appears in the frr_key dataframe
## pred.set: character, which predictor set to use
## only.nolte.counties: logical, whether to subset data to only counties that 
## are modeled by Nolte's base county model

frr_model <- function(frr, pred.set = c("full", "nolte"), only.nolte.counties) {
  
  tic(frr)
  
  # Read in data ==============================================================
  frr_data <-
    # Only read in data from counties within the FRR boundary
    read_frr_data(
      frr = frr, only.nolte.counties = only.nolte.counties
    )
  
  # Select model predictors ===================================================
  
  # Determine which predictors to use, depending on Full or Nolte set
  model_predictors <-
    predictor_set(geo = "frr", pred.set = pred.set, frr_data)
  
  # Select only those predictors, and clean up coastal variables
  model_data <-
    frr_data %>%
    frr_feature_selection(model_predictors = model_predictors) %>%
    
    # Since different states have different soil variables, we must replace 
    # soil NAs with 0 to avoid dropping every row when we call na.omit()
    mutate(
      across(.cols = any_of(soil_vars), .fns = ~ replace_na(.x, 0))
      ) %>%
    # Remove NA rows
    na.omit()
  
  # Fit FRR model =============================================================
  frr_rf_fit <- rf_fit(geo = "frr", frr, model_data)

  # Extract model results =====================================================
  
  # Results: 
  #   1. predictions (every sales record),
  #   2. predictions (test set only)
  #   3. performance metrics
  #   4. variable importance
  
  frr_results <- 
    extract_results(geo = "frr", frr_rf_fit, frr) %>%
    sort_list()
  
  # Save FRR-level model results ==============================================
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
  
  toc()
  
  }
