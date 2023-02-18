# model_state_counties() =========================================================
#
# Runs all county models within a given state 
# 
## state: character vector, uppercase two-letter abbreviation
## pred.set: which predictor set to use
## HPI: logical, whether to include HPI in the Nolte predictor set

model_state_counties <- function(state, pred.set, HPI) {

  tic(state)
  
  # Read in data =============================================================
  
  # Read in current state's data
  state_data <- read_state_clean(state)
  
  # Split state data into list where each element is a county's dataset
  counties_data_list <-
    state_data %>%
    split(.$fips)
  
  # Split state data into list where each element is a county's NEIGHBORS' data
  neighbors_data_list <-
    find_neighbors(state_data)

  # Define model predictors
  model_predictors <- 
    predictor_set(
      data = state_data, geo = "county", 
      pred.set = pred.set, HPI = HPI)
  
  # If no data, exit function
  if (nrow(state_data) == 0) {
    message('\nNo obs in `', state, "`. Moving on to next state.", sep = "")
    return()
  } else {
    # Prep parallel workers
    unregisterCores()
    if (foreach::getDoParWorkers() < 32) doParallel::registerDoParallel(32)
  }
    
  # Build models for all counties in state ====================================
  
  state_rf_fit <-
    foreach::foreach(
      county_data = counties_data_list,
      neighbor_data = neighbors_data_list) %dopar% {
        
        # Fit model
        rf_fit(geo = "county", 
               county_data = county_data, 
               neighbor_data = neighbor_data, 
               model_predictors)
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
      extract_results(geo = "county", state_rf_fit) %>%
      sort_list()
     
    # Save state-level county model results ===================================
    result_paths <-
      make_results_path(
        geo = "county",
        state = state,
        model_abbr = make_model_abbr(geo = "county", pred.set, HPI)
        ) %>%
      sort_list()
    
    walk2(
      .x = state_results,
      .y = result_paths,
      .f = ~write_parquet_verbose(.x, .y)
    )
    
    toc()
}

# Helper functions ============================================================


# Creates list of each county's neighbor datasets ================
find_neighbors <- function(state_data) {
  
  focal_counties <- unique(state_data$fips)
  
  neighbor_list <-
    focal_counties %>%
    map(
      ~ filter(state_data, fips %in% county_neighbors[[.x]])
    ) %>%
    set_names(focal_counties)
  
  neighbor_list
  
}

# Sort list alphabetically by names
sort_list <- function(list) list[sort(names(list))]

