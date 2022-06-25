
# Set-up ####

## Load Packages ####
# install.packages(c('tidymodels','usemodels','vip','ranger'))
library(tidymodels)
library(usemodels)
library(vip)
library(doParallel)
library(showtext)
library(sysfonts)
library(magrittr)
library(readr)
library(arrow)
tidymodels_prefer()

source('/home/rstudio/users/gold1/fmv/code/custom_functions.R')
source("Y://code/custom_functions.R")

## Load County Adjacency df ####
county_adjacency <- readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv")

## Set working directory ####
setwd("/home/rstudio/users/gold1/fmv/data/cleaned")
setwd("Y://data/cleaned")

## Vector of all state pqt files ####
all_clean <- list.files()

start <- Sys.time()
# Loop through States ####
for (i in seq_len(length(all_clean))) {
  
  ## Build empty stats dataframes ####
  
  ### Predictions ####
  state_predictions <- tibble(
    id = NA, .pred = NA,
    .row = NA, logpriceadj_ha = NA,
    .config = NA, fips = NA
  ) %>%
    slice(0)
  
  ### Performance Stats ####
  collect_stats_rf <- 
    tibble(rmse = NA, rsq = NA, mse = NA,
           fips = NA, nobs = NA, percent_neighbor = NA) %>% 
    slice(0)
  
  
  ### Variable Importance ####
  state_importance <- arrow::read_parquet(all_clean[[12]]) %>%
    names() %>%
    tibble(Variable = .,
           na = NA) %>%
    pivot_wider(
      names_from = Variable,
      values_from = na
    ) %>%
    slice(0)
  
  
  ## Current state ####
  state <- stringr::str_extract(all_clean[[i]], "[:upper:]{2}")
  
  ## Import current state df ####
  df_import <- arrow::read_parquet(all_clean[[i]]) %>%
    dplyr::select(!HPI)
  
  
  ## Specify current state counties ####
  state_counties <- df_import %>%
    pull(fips) %>%
    unique()
  
  
  ## Loop through all counties ####
  
  for (j in seq_len(length(state_counties))) {
    
    ### Subset to current county ####
    county_df <- df_import %>%
      dplyr::filter(fips==state_counties[[j]]) 
    
    ### Vector of neighbors ####
    neighbors <- county_adjacency %>%
      dplyr::filter(countyname != neighborname) %>%  
      dplyr::filter(fipscounty == state_counties[[j]]) %>%
      dplyr::pull(fipsneighbor)
    
    
    
    ### Specify df for modeling
    
    
    if (nrow(county_df) < 1000) {
      
      rows_needed <- 1000 - nrow(county_df)
      
      neighbor_df <- df_import %>%
        dplyr::filter(fips %in% neighbors)
      
      if(rows_needed <= nrow(neighbor_df)) {
        
        model_df <- dplyr::bind_rows(county_df,
                                     neighbor_df %>% 
                                       slice_sample(n = rows_needed))
        
      } else {
        
        model_df <- county_df
        
      } 
    } else {
      model_df <- county_df
    }
    
    
    
    if(nrow(model_df)>=1000) {
      
      ## Modeling ####
      
      rf_data <- model_df %>%
        select(!c(sid, fips)) %>%
        stats::na.omit()
      
      
      ### Construct Model ####
      
      # split data
      set.seed(319)
      rf_split <- rsample::initial_split(rf_data, strata = log_priceadj_ha)
      train <- rsample::training(rf_split)
      test <- rsample::testing(rf_split)
      
      
      # build resamples for cross-validation during tuning
      set.seed(194)
      
      rf_folds <- rsample::vfold_cv(train, 
                                    strata = log_priceadj_ha) # how many folds should we do?
      
      
      ### Model Workflow ####
      
      #### Formula and Preprocessing ####
      ranger_recipe <- 
        recipes::recipe(formula = log_priceadj_ha ~ ., 
                        data = train)
      
      #### Engine, Mode, Method ####
      ranger_spec <- 
        parsnip::rand_forest(mtry = tune(), 
                             min_n = tune(), # set to 3 later
                             trees = 500) %>% 
        set_mode("regression") %>% 
        set_engine("ranger", 
                   importance = "permutation")
      
      #### Build Workflow ####
      ranger_workflow <- 
        workflow() %>% 
        add_recipe(ranger_recipe) %>% # the recipe in the formula
        add_model(ranger_spec)  # the parameters, engine, importance methods, etc.
      
      
      
      ### Hyperparameter Tuning ####
      
      
      # Parallel Process
      set.seed(15224)
      if(getDoParWorkers()<64) {
        
        unregister()
        registerDoParallel(64)
        
      }
      
      # Tune
      ranger_tune <- tune_grid(ranger_workflow,
                               resamples = rf_folds)
      
      # Select Best
      final_rf <- ranger_workflow %>%
        finalize_workflow(select_best(ranger_tune, 
                                      metric = "rmse"))
      
      
      ### Model Fitting ####
      rf_fit <- last_fit(final_rf, rf_split)
      
      
      ### Collect Performance Stats ####
      
      n_obs <- nrow(model_df)
      
      n_neighbors_rf <- model_df %>%
        dplyr::filter(fips != state_counties[[j]]) %>%
        nrow()
      
      county_stats_rf <- collect_metrics(rf_fit) %>%
        select(.metric, .estimate) %>%
        spread(.metric, .estimate) %>%
        mutate(
          mse = rmse^2,
          fips = state_counties[[j]],
          nobs = n_obs,
          percent_neighbor = n_neighbors_rf/nobs) 
      
      
      collect_stats_rf <- collect_stats_rf %>%
        rbind(county_stats_rf)
      
      
      ### Collect Predictions ####
      
      county_predictions <- collect_predictions(rf_fit) %>%
        mutate(fips = state_counties[[j]])
      
      state_predictions <- rbind(state_predictions,
                                 county_predictions)
      
      
      
      ### Variable Importance ####
      
      county_importance <- final_rf %>%
        fit(test) %>%
        extract_fit_parsnip() %>%
        vip::vi() %>%
        add_row(Variable = "fips",
                Importance = as.double(state_counties[[j]])) %>%
        pivot_wider(
          names_from = Variable,
          values_from = Importance
        ) %>%
        relocate(fips)
      
      state_importance <- rbind(state_importance, county_importance)
      
      cat("Complete: ", state_counties[[j]], 
          " |.....| Modeled: YES, n.obs: ", nrow(model_df), 
          " |.....| % Neighbors: ", 
          county_stats_rf$percent_neighbor, "\n",
          sep = "")
      
    } else {
      
      cat("Complete: ", state_counties[[j]], 
          " |.....| Modeled:  NO, n.obs: ", 
          nrow(model_df)+nrow(neighbor_df), "\n",
          sep = "")
      
    }
    
  }
 
  
  ## Write state-level model stats to file ####

    write_parquet(state_predictions, 
                  paste0("predictions/pred_",state, ".pqt"))
    
    write_parquet(collect_stats_rf, 
                  paste0('performance/stats_', state, ".pqt"))
    
    write_parquet(state_importance, 
                  paste0('importance/import_', state, ".pqt"))
    
    cat("\n\nFinished: ", state, "\n\n", sep = "")
    
    end <- Sys.time()
    
  }
  







