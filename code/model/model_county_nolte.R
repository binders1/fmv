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


source('~/fmv/code/custom_functions.R')

## Load County Adjacency df ####
county_adjacency <- readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
                                    show_col_types = F)


## Load Nolte (2020) features

setwd("~/fmv/data")
nolte2020vars <- read_csv("nolte2020vars.csv",
                          show_col_types = F) %>% 
  pull()

## Set working directory ####
setwd("~/fmv/data/cleaned")

## Vector of all state pqt files ####
all_clean <- list.files()

fitRF <- function(j) {
  
  HPI_na <- sum(is.na(df_import$HPI))
  
  nrow_county <- sum(df_import$fips==state_counties[[j]])
  
  if (HPI_na >= nrow_county) {
    
    ### Subset to current county ####
    
    county_df <- df_import %>%
      dplyr::filter(fips==state_counties[[j]]) %>%
      dplyr::select(!HPI)
    
  } else {
    
    county_df <- df_import %>%
      dplyr::filter(fips==state_counties[[j]])
    
  }
  
  ### Vector of neighbors ####
  neighbor_vec <- county_adjacency %>%
    dplyr::filter(countyname != neighborname) %>%  
    dplyr::filter(fipscounty == state_counties[[j]]) %>%
    dplyr::pull(fipsneighbor)
  
  
  ### Specify df for modeling
  
  
  if (nrow(county_df) < 1000) {
    
    rows_needed <- 1000 - nrow(county_df)
    
    neighbor_df <- df_import %>%
      dplyr::filter(fips %in% neighbor_vec) %>%
      dplyr::select(dplyr::any_of(names(county_df)))
    
    if(rows_needed <= nrow(neighbor_df)) {
      
      model_df <- dplyr::bind_rows(county_df,
                                   neighbor_df %>% 
                                     slice_sample(n = rows_needed)) %>%
        dplyr::select(log_priceadj_ha, fips, sid, dplyr::any_of(nolte2020vars)) %>%
        stats::na.omit()
      
      mod_nrow <- nrow(dplyr::bind_rows(county_df,
                                   neighbor_df %>% 
                                     slice_sample(n = rows_needed)) %>%
        dplyr::select(log_priceadj_ha, fips, sid, dplyr::any_of(nolte2020vars)))
      
    } else {
      
      model_df <- county_df %>%
        dplyr::select(log_priceadj_ha, fips, sid, dplyr::any_of(nolte2020vars)) %>%
        stats::na.omit()
      
      mod_nrow <- nrow(
        county_df %>%
          dplyr::select(log_priceadj_ha, fips, sid, dplyr::any_of(nolte2020vars))
      )
      
    } 
    
  } else {
    
    model_df <- county_df %>%
      dplyr::select(log_priceadj_ha, fips, sid, dplyr::any_of(nolte2020vars)) %>%
      stats::na.omit()
    
    
    mod_nrow <- nrow(
      county_df %>%
        dplyr::select(log_priceadj_ha, fips, sid, dplyr::any_of(nolte2020vars))
    )
    
  }
  
  
  
  if(mod_nrow>=1000) {
    
    ## Modeling ####
    
    rf_data <- model_df %>%
      dplyr::select(!c(sid, fips)) 
    
    
    ### Construct Model ####
    
    # split data
    set.seed(319)
    
    tryCatch(
      
      rf_split <- rsample::initial_split(rf_data, 
                                         strata = log_priceadj_ha),
      
      error = function(e)
        cat('Model split error in:',state_counties[[j]])
      
    )
    

    train <- rsample::training(rf_split)
    test <- rsample::testing(rf_split)
    
    ### Model Workflow ####
    
    #### Formula and Preprocessing ####
    ranger_recipe <- 
      recipes::recipe(formula = log_priceadj_ha ~ ., 
                      data = train)
    
    #### Engine, Mode, Method ####
    ranger_spec <-
      parsnip::rand_forest(mtry = length(names(train))/3, 
                           min_n = 3, # increase required sample leaf size to avoid overfitting
                           trees = 500) %>% # try 250 trees
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("ranger",
                          splitrule = "extratrees",
                          importance = "permutation")
    
    #### Build Workflow ####
    ranger_workflow <- 
      workflows::workflow() %>% 
      workflows::add_recipe(ranger_recipe) %>% # the recipe in the formula
      workflows::add_model(ranger_spec) # the parameters, engine, importance methods, etc.
    
    ### Model Fitting ####
    
    tryCatch(
      
      rf_fit <- tune::last_fit(ranger_workflow, rf_split) %>%
        dplyr::mutate(fips = state_counties[[j]],
                      n_obs = nrow(rf_data),
                      n_county = nrow(model_df %>% dplyr::filter(fips == state_counties[[j]])),
                      n_neighbor = nrow(model_df %>% dplyr::filter(fips != state_counties[[j]]))),
      
      error = function(e)
        warning('Model fit error in county', state_counties[[j]])
      
      
    )
    
    
    return(rf_fit)

  } else {
    return()
  }
  
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
      cat('\n\n Data import error occured in', state)
    
  )
  
  ## Specify current state counties ####
  state_counties <- df_import %>%
    pull(fips) %>%
    unique()
  
  
  ## County Models ####
  
  unregister()
  
  if(foreach::getDoParWorkers()<64) {
    
    doParallel::registerDoParallel(64)
    
  }
  
  state_rf_fit <- foreach::foreach(j=seq_len(length(state_counties))) %dopar% {
    
    tryCatch(
      
      fitRF(j),
      
      error = function(e)
        cat('\n\n Error occured in county \n\n', 
            state_counties[[j]])
      
    )
    
  } %>%
    bind_rows()
  
  unregister()
  
  
  
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
  setwd("~/fmv/data/model/county/nolte")
  write_parquet(state_predictions, 
                paste0("predictions/pred_",state, ".pqt"))
  
  write_parquet(state_stats, 
                paste0('performance/stats_', state, ".pqt"))
  
  write_parquet(state_importance, 
                paste0('importance/import_', state, ".pqt"))
  
  cat('\n\n Finished:',state, "\n\n")
  
}