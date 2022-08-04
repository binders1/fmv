
# Load pkgs ####
library(tidymodels)
library(tidyverse)
library(usemodels)
library(vip)
library(doParallel)
library(showtext)
library(sysfonts)
library(magrittr)
library(readr)
library(arrow)
tidymodels_prefer()


# Source custom functions ####
source("~/fmv/code/functions/sourceFuncs.R")
sourceFuncs()

## Load County Adjacency df ####
county_adjacency <- readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
                                    show_col_types = F)

## Set working directory ####
setwd("~/fmv/data/cleaned")

## Vector of all state pqt files ####
all_clean <- list.files()

if(foreach::getDoParWorkers()<64) {
  
  doParallel::registerDoParallel(64)
  
}


for (i in 1:49) {
  # (re)set working directory to clean data folder
  setwd("~/fmv/data/cleaned")
  
  ## Current state ####
  state <- stringr::str_extract(all_clean[[i]], "[:upper:]{2}")
  
  
  cat('\n Trying:',state, '\n')
  
  tryCatch(
    
    ## Import current state df ####
    df_import <- arrow::read_parquet(all_clean[[i]]),
    
    error = function(e)
      cat('\n\n Error occured in', state)
    
  )
  
  ## Specify current state counties ####
  state_counties <- df_import %>%
    pull(fips) %>%
    unique()
  
  
  ## County Models ####
  
  state_rf_fit <- foreach::foreach(j=seq_len(length(state_counties))) %dopar% {
    
    tryCatch(
      
      fitRF(j),
      
      error = function(e)
        cat('\n\n Error occured in county', 
            state_counties[[j]])
      
    )
    
  } %>%
    bind_rows()
  
  
  
  ## Collect Pred/Stats/Import ####
  
  state_predictions <- map(.x = seq_len(nrow(state_rf_fit)),
                           .f = ~ state_rf_fit$.predictions[[.x]] %>%
                             dplyr::mutate(fips = state_rf_fit$fips[[.x]])) %>% bind_rows()
  
  state_stats <- map(.x = seq_len(nrow(state_rf_fit)),
                     .f = ~ state_rf_fit$.metrics[[.x]] %>%
                       dplyr::select(.metric, .estimate) %>%
                       dplyr::mutate(fips = state_rf_fit$fips[[.x]]) %>%
                       pivot_wider(
                         names_from = .metric,
                         values_from = .estimate
                       ) %>%
                       mutate(mse = rmse^2,
                              n_train = state_rf_fit$.workflow[[.x]] %>% 
                                extract_fit_engine() %>%
                                .$num.samples,
                              n_obs = state_rf_fit$n_obs[[.x]],
                              n_test = n_obs - n_train)) %>% bind_rows()
  
  state_importance <- map(.x = seq_len(nrow(state_rf_fit)), 
                          .f = 
                            ~ state_rf_fit$.workflow[[.x]] %>%
                            extract_fit_parsnip() %>%
                            vip::vi() %>%
                            pivot_wider(
                              names_from = Variable,
                              values_from = Importance) %>%
                            mutate(fips = state_rf_fit$fips[[.x]])) %>% bind_rows()
  
  
  
  
  ## Write state-level model stats to file ####
  setwd("~/fmv/data/model/county/rf")
  write_parquet(state_predictions, 
                paste0("predictions/pred_",state, ".pqt"))
  
  write_parquet(state_stats, 
                paste0('performance/stats_', state, ".pqt"))
  
  write_parquet(state_importance, 
                paste0('importance/import_', state, ".pqt"))
  
  cat('\n\n Finished:',state, "\n\n")
  
}






