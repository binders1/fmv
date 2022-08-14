#===================================#
#
#  Full County Model             ####
#
#===================================#


# Set up ####

## Load pkgs ####
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


## Set dirs ####

root <- "~/fmv"

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
odir <- file.path(root, "output")

f_dir <- file.path(cdir, "functions")
m_dir <- file.path(cdir, "model")

clean_dir <- file.path(ddir, "cleaned")



## Source custom functions ####
walk(
  file.path(f_dir, list.files(f_dir)),
  source
)

## Load County Adjacency df ####
county_adjacency <- readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
                                    show_col_types = F)


## Vector of all state pqt files ####
all_clean <- list.files(clean_dir)


# Loop through all 49 states ####
for (i in 1:49) {
  
  # (re)set working directory to clean data folder
  clean_path <- file.path(clean_dir, all_clean)
  
  ## Current state ####
  state <- stringr::str_extract(all_clean[[i]], "[:upper:]{2}")
  
  
  cat('\n Trying:',state, '\n')
  
  tryCatch(
    
    ## Import current state df ####
    df_import <- arrow::read_parquet(clean_path[[i]]),
    
    error = function(e)
      cat('\n\n Data import error occured in', state)
    
  )
  
  ## Specify current state counties ####
  state_counties <- df_import %>%
    pull(fips) %>%
    unique()
  
  if (nrow(df_import) > 0) {
    
    ## County Models ####
    
    unregisterCores()
    
    if(foreach::getDoParWorkers()<64) {
      
      doParallel::registerDoParallel(64)
      
    }
    
    state_rf_fit <- 
      foreach::foreach(j=seq_len(length(state_counties))) %dopar% {
        
        tryCatch(
          
          fitRF(j, 
                everything()),
          
          error = function(e)
            cat('\n\n Error occured in county \n\n', 
                state_counties[[j]])
          
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
          rbind(
            state_test,
            current_fit
          )
        
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