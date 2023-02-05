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
        
        tryCatch(
          rf_fit(county, any_of(model_predictors)),
          
          error = function(e)
            message('\nError occured in county', state_counties[[j]])
        )
        
      }
    
    unregisterCores()
    
    
    ### If not models specified, move on ####
    null_mods <- 
      map_lgl(
        state_rf_fit,
        is.null
      ) %>%
      sum()
    
    if (null_mods == length(state_counties)) {
      
      cat("\n No models specified in", state, ". Moving on...")
      
    } else {
      
      ### Extract Test Set and Mod Fit ####
      
      #### Test Set ####
      state_test <- tibble()
      
      for (k in seq_len(length(state_rf_fit))) {
        
        current_fit <- state_rf_fit[[k]][[1]]
        
        if (!("HPI" %in% names(current_fit)) &
            !is.null(current_fit)) {
          
          current_fit %<>%
            mutate(HPI = NA)
          
        }
        
        state_test <-
          rbind(state_test, current_fit)
        
      }
      
      #### State models ####
      state_mod <- tibble()
      
      for (i in seq_len(length(state_rf_fit))) {
        
        state_mod <-
          rbind(
            state_mod,
            state_rf_fit[[i]][[2]]
          )
        
      }
      
      
      ## Collect Pred/Stats/Import ####
      
      state_predictions <- 
        state_mod %>% 
        select(.predictions) %>% 
        unnest(.predictions) %>%
        select(.pred) %>%
        bind_cols(., state_test)
      
      state_stats <-
        state_mod %>% 
        select(.metrics, fips) %>% 
        unnest(.metrics) %>%
        select(.metric, .estimate, fips) %>%
        pivot_wider(
          names_from = .metric,
          values_from = .estimate) %>%
        mutate(mse = rmse^2,
               n_train = map_int(seq_len(nrow(state_mod)), 
                                 ~ state_mod$.workflow[[.x]] %>%
                                   extract_fit_engine() %>%
                                   .$num.samples),
               n_obs = state_mod$n_obs,
               n_test = n_obs - n_train,
               n_neighbor = state_mod$n_neighbor)
      
      state_importance <- 
        map(.x = seq_len(nrow(state_mod)), 
            .f = ~ state_mod$.workflow[[.x]] %>%
              extract_fit_parsnip() %>%
              vip::vi() %>%
              pivot_wider(
                names_from = Variable,
                values_from = Importance) %>%
              mutate(fips = state_mod$fips[[.x]])) %>% 
        bind_rows()
      
      
      ## Write state-level model stats to file ####
      fcb_dir <- file.path(ddir, "model/full/county/base")
      
      pred_dir <- file.path(fcb_dir, "predictions")
      perform_dir <- file.path(fcb_dir, "performance")
      imp_dir <- file.path(fcb_dir, "importance")
      
      pred_file <- paste0("pred_fcb_", state, ".pqt")
      perform_file <- paste0('stats_fcb_', state, ".pqt")
      imp_file <- paste0('import_fcb_', state, ".pqt")
      
      
      write_parquet(state_predictions, 
                    file.path(pred_dir, pred_file))
      
      write_parquet(state_stats, 
                    file.path(perform_dir, perform_file))
      
      write_parquet(state_importance, 
                    file.path(imp_dir, imp_file))
      
      cat('\n\n Finished:',state, "\n\n")
      
    }
    
    
  } else {
    
    cat('\n\n No obs in: ',state, ". Moving on...\n\n", sep = "")
    
  }
  
}