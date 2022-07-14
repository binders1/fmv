
# Set-up ####

## Load Packages ####
library(MASS)
library(broom)
library(tidyverse)
library(magrittr)
library(arrow)


## Load custom functions ####
walk(paste0("~/fmv/code/functions/", 
            list.files("~/fmv/code/functions")), 
     source)

## Load County Adjacency df ####
county_adjacency <- 
  readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
                  show_col_types = F)

## Set working directory ####
setwd("~/fmv/data/cleaned")

## Vector of all state pqt files ####
all_clean <- list.files()

max_features <- 
  map(all_clean,
      ~ read_parquet(.x) %>%
        names() %>%
        length()) %>%
  unlist() %>%
  max()

# Loop through States ####
for (i in seq_len(length(all_clean))) {
  
  ## Reset working directory
  setwd("~/fmv/data/cleaned")
  
  ## Current state ####
  state <- 
    str_extract(all_clean[[i]], "[:upper:]{2}")
  
  ## Import current state df ####
  df_import <- 
    read_parquet(all_clean[[i]])
  
  
  ## Specify current state counties ####
  state_counties <- 
    df_import %>%
      pull(fips) %>%
      unique()
  
  
  ## Build empty stats dataframes ####
  
  ### BASE Predictions ####
  collect_pred_base <- 
    tibble(.pred = NA, log_priceadj_ha = NA, fips = NA) %>%
      slice(0)
  
  collect_pred_test <- 
    tibble(.pred = NA, log_priceadj_ha = NA, fips = NA) %>%
      slice(0)

  
  ### BASE Performance Stats ####
  
  collect_stats_base <- 
    tibble::tibble(r.squared = NA, adj.r.squared = NA, sigma = NA,
                   statistic = NA, p.value = NA, df = NA, logLik = NA,
                   AIC = NA, BIC = NA, deviance = NA, df.residual = NA,
                   nobs = NA, rmse = NA, fips = NA, percent_neighbor = NA) %>% 
    slice(0)
  
  
  ### BASE Variable Importance ####
  collect_import_base <- 
    tibble(name = c(paste0('estimate!!', names(df_import)),
                    paste0("std.error!!", names(df_import))),
           na = NA) %>%
    separate(col = name, into = c('1','2'), sep ="!!") %>%
    arrange(`2`) %>%
    mutate(name = paste(`1`, `2`, sep = "!!"), 
           .keep = "unused") %>%
    pivot_wider(names_from = name, values_from = na) %>%
    slice(0)
  
  ### TEST Predictions ####
  collect_stats_test <- 
    tibble(.pred = NA, log_priceadj_ha = NA, fips = NA) %>%
    slice(0)
  
  
  ### TEST Performance Stats ####
  
  collect_stats_test <- 
    tibble::tibble(r.squared = NA, mse = NA, nobs = NA, 
                   fips = NA, percent_neighbor = NA) %>% 
    slice(0)
  
  
  ### AIC-selected Variables ####
  
  collect_AIC_vars <- 
    tibble(rowid = seq_len(max_features))

  
  
  ## Loop through all counties ####
  
  for (j in seq_len(length(state_counties))) {
    
    county_df <- 
      df_import %>%
        dplyr::filter(fips == state_counties[[j]])
    
    HPI_na <- 
      sum(is.na(county_df$HPI))
    
    nrow_county <- 
      sum(df_import$fips==state_counties[[j]])
    
    if(HPI_na==nrow_county) {
      ### Subset to current county ####
      
      county_df <- 
        df_import %>%
          dplyr::filter(fips==state_counties[[j]]) %>%
          dplyr::select(!HPI)
      
    } else {
      
      county_df <- 
        df_import %>%
          dplyr::filter(fips==state_counties[[j]])
      
    }
    
    ### Vector of neighbors ####
    neighbors <- 
      county_adjacency %>%
        dplyr::filter(countyname != neighborname) %>%  
        dplyr::filter(fipscounty == state_counties[[j]]) %>%
        dplyr::pull(fipsneighbor)
    
    
    
    ### Specify df for modeling
    
    
    if (nrow(county_df) < 1000) {
      
      rows_needed <- 1000 - nrow(county_df)
      
      neighbor_df <- 
        df_import %>%
         dplyr::filter(fips %in% neighbors)
      
      if(rows_needed <= nrow(neighbor_df)) {
        
        model_df <- 
          dplyr::bind_rows(county_df,
                           neighbor_df %>%
                             slice_sample(n = rows_needed)
                           )
        
      } else {
        
        model_df <- county_df
        
      } 
      
    } else {
      
      model_df <- county_df
    
      }
    
    
    
    if(nrow(model_df)>=1000) {
      
      
      ## Training testing split ####
      
      reg_split <- 
        rsample::initial_split(model_df)
      
      training <- 
        rsample::training(reg_split) %>%
        dplyr::select(!c(sid, fips)) %>%
        na.omit()
      
      testing <- 
        rsample::testing(reg_split) %>%
        dplyr::select(!c(sid, fips)) %>%
        na.omit()
      
      
      
      ## Modeling ####
      
      ### Regression on training data ####
      
      baseMod <- 
        lm(log_priceadj_ha ~ ., data = training )
      
      ### AIC variable selection ####
      
      stepMod <- 
        stepAIC(baseMod,
                trace = FALSE,
                direction = "backward")
      
      ### Evaluate AIC model on testing data ####
       
      pred_step <- 
        predict(stepMod, testing)
      
      test_pred <- 
        testing %>%
        mutate(.pred = pred_step) %>%
        dplyr::select(log_priceadj_ha, .pred) %>%
        mutate(resid_sq = (log_priceadj_ha - .pred)^2)
      
      aic_vars <- 
        names(stepMod$model) %>%
        tibble() %>%
        rename(!!state_counties[[j]] := 1) %>%
        rowid_to_column()
      
      ### Collect Model Performance ####
      
      #### Pre-calculations ####
      
      # MSE #
      mse_base <- 
        mean(baseMod$residuals^2)
      
      mse_test <- 
        mean(test_pred$resid_sq)
      
      # R-Squared (test) #
      
      tss_test <- sum((test_pred$log_priceadj_ha - mean(test_pred$log_priceadj_ha))^2)
      
      rsq_test <- 1 - (sum(test_pred$resid_sq)/tss_test)
      
      # Percent obs from neighbors #
      n_neighbors <- 
        model_df %>%
        filter(fips != state_counties[[j]]) %>%
        nrow()
      
      percent_neighbors <- 
        n_neighbors/nrow(model_df)
      
      
      #### Performance stats ####
      
      # BASE #
      county_stats_base <- 
        broom::glance(baseMod) %>%
        mutate(mse = mse_base,
               fips = state_counties[[j]],
               percent_neighbors = percent_neighbors)
      
      collect_stats_base <- 
        rbind(collect_stats_base, county_stats_base)
      
      # TEST #
      county_stats_test <- 
        tibble(r.squared = rsq_test, 
               mse = mse_test, nobs = nrow(testing), 
               percent_neighbors = percent_neighbors, 
               fips = state_counties[[j]])
      
      collect_stats_test <- 
        rbind(collect_stats_test, county_stats_test)
      
      #### Predictions ####
      
      # BASE #
      county_pred_base <- 
        tibble(.pred = baseMod$fitted.values,
               log_priceadj_ha = baseMod$model[,c('log_priceadj_ha')],
               fips = state_counties[[j]])

      collect_pred_base <- 
        rbind(collect_pred_base, county_pred_base)
      
      # TEST #
      county_pred_test <- 
        test_pred %>%
        dplyr::select(log_priceadj_ha, .pred) %>%
        mutate(fips = state_counties[[j]])
      
      collect_pred_test <- 
        rbind(collect_pred_test, county_pred_test)
        
      #### Variable Importance ####
      
      # BASE #
      county_import_base <- 
        broom::tidy(baseMod) %>%
        dplyr::select(term, estimate, std.error, p.value) %>%
        filter(term != "(Intercept)") %>%
        mutate(fips = state_counties[[j]]) %>%
        pivot_wider(
          names_from = term,
          values_from = c(estimate, std.error, p.value),
          names_sep = "!!"
        )

      collect_import_base <- 
        rbind(collect_import_base, county_import_base)
      
      
      
      #### AIC-selected variables ####
      
      collect_AIC_vars <- collect_AIC_vars %>%
        left_join(aic_vars, by = c('rowid'))
      
      
      cat("Complete: ", state_counties[[j]], 
          " |.....| Modeled: YES, n.obs: ", nrow(model_df), 
          " |.....| % Neighbors: ", 
          county_stats_base$percent_neighbors, "\n",
          sep = "")
      
    } else {
      
      cat("Complete: ", state_counties[[j]], 
          " |.....| Modeled:  NO, n.obs: ", 
          nrow(model_df)+nrow(neighbor_df), "\n",
          sep = "")
      
    }
    
  }
  
  
  ## Write state-level model stats to file ####
  
  
  ### BASE ####
  
  setwd('~/fmv/data/model/county/reg/base')
  
  #### Performance Stats ####
  write_parquet(collect_stats_base, 
                paste0("performance/stats_base_",state, ".pqt"))
  
  #### Predictions ####
  write_parquet(collect_pred_base, 
                paste0('predictions/pred_base_', state, ".pqt"))
  
  #### Importance ####
  write_parquet(collect_import_base, 
                paste0('importance/import_base_', state, ".pqt"))
  
  
  ### TEST (AIC) ####
  
  setwd('~/fmv/data/model/county/reg/test')
  
  #### Performance Stats ####
  write_parquet(collect_stats_test, 
                paste0("performance/stats_test_",state, ".pqt"))
  
  #### Predictions ####
  write_parquet(collect_pred_test, 
                paste0('predictions/pred_test_', state, ".pqt"))
  
  #### AIC-selected variables ####
  write_parquet(collect_AIC_vars,
                paste0('AIC_vars/AIC_vars_', state, ".pqt"))

  
  cat("\n\nFinished: ", state, "\n\n", sep = "")
  
}
