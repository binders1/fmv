# fmv_state_model() =========================================================
#
# Runs all county models within a given state 
# 
## state: character vector, uppercase two-letter abbreviation
## pred.set: which predictor set to use
## HPI: whether to include HPI in the Nolte predictor set

fmv_state_model <- function(state, pred.set = c("full", "nolte"), HPI = TRUE) {
  
  # Read in data =============================================================
  
  # Read in current state's data
  state_data <- read_state_clean(state)
  
  # Vector of current state's counties
  state_counties <- unique(state_data$fips)
  
  # If no data, exit function
  if (nrow(state_data) == 0) {
    message('\nNo obs in `', state, "`. Moving on to next state.", sep = "")
    return()
  } else {
    # Prep parallel workers
    unregisterCores()
    if (foreach::getDoParWorkers() < 64) doParallel::registerDoParallel(64)
  }
  
  model_predictors <- 
    predictor_set(
      data = state_data, geo = "county", 
      pred.set = pred.set, HPI = HPI)
    
    # Build models for all counties in state =================================
    
  state_rf_fit <-
    foreach::foreach(county = state_counties) %dopar% {
      rf_fit(county, model_predictors)
      }
  
    unregisterCores()
    
    # If not models specified, exit ===========================================
    no_models_specified <-
      all(map_lgl(state_rf_fit, is.null))
    
    if (no_models_specified) return(message("No models specified in ", state, ". Moving on..."))
    
    # If models were specified, extract results ===============================
    
    # Results: 
    #   1. predictions (every sales record),
    #   2. predictions (test set only)
    #   3. performance metrics
    #   4. variable importance
    state_results <-
      state_rf_fit %>%
      extract_state_results() %>%
      sort_list()
     
    # Save state-level county model results ===================================
    result_paths <-
      make_results_path.state(
        state = state,
        model_abbr = make_model_abbr(pred.set, HPI)
        ) %>%
      sort_list()
    
    walk2(
      .x = state_results,
      .y = result_paths,
      .f = ~write_parquet_verbose(.x, .y)
    )
}

# Helper functions ============================================================
sort_list <- function(list) list[c(sort(names(list)))]

write_parquet_verbose <- function(x, sink) {
  write_parquet(x, sink)
  message("\nWrote to:\n", sink)
}

