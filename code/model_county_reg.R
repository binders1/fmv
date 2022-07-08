
# Set-up ####

## Load Packages ####
library(MASS)
library(broom)
library(tidyverse)
library(magrittr)
library(arrow)


## Load custom functions ####
source('/home/rstudio/users/gold1/fmv/code/custom_functions.R')


## Load County Adjacency df ####
county_adjacency <- 
  readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
                  show_col_types = F)

## Set working directory ####
setwd("/home/rstudio/users/gold1/fmv/data/cleaned")

## Vector of all state pqt files ####
all_clean <- list.files()

start <- Sys.time()
# Loop through States ####
for (i in seq_len(length(all_clean))) {
  
  ## Reset working directory
  setwd("/home/rstudio/users/gold1/fmv/data/cleaned")
  
  ## Current state ####
  state <- str_extract(all_clean[[i]], "[:upper:]{2}")
  
  ## Import current state df ####
  df_import <- read_parquet(all_clean[[i]]) %>%
    dplyr::select(!HPI)
  
  
  ## Specify current state counties ####
  state_counties <- df_import %>%
    pull(fips) %>%
    unique()
  
  
  ## Build empty stats dataframes ####
  
  ### Predictions ####
  collect_pred_reg <- tibble(.pred = NA, log_priceadj_ha = NA, fips = NA) %>%
    slice(0)

  
  ### Performance Stats ####
  
  collect_stats_base <- 
    tibble::tibble(r.squared = NA, adj.r.squared = NA, sigma = NA,
                   statistic = NA, p.value = NA, df = NA, logLik = NA,
                   AIC = NA, BIC = NA, deviance = NA, df.residual = NA,
                   nobs = NA, rmse = NA, fips = NA, percent_neighbor = NA) %>% 
    slice(0)
  
  
  ### Variable Importance ####
  collect_import_reg <- 
    tibble(name = c(paste0('estimate!!', names(df_import)),
                    paste0("std.error!!", names(df_import))),
           na = NA) %>%
    separate(col = name, into = c('1','2'), sep ="!!") %>%
    arrange(`2`) %>%
    mutate(name = paste(`1`, `2`, sep = "!!"), .keep = "unused") %>%
    pivot_wider(names_from = name, values_from = na) %>%
    slice(0)

  
  
  ## Loop through all counties ####
  
  for (j in seq_len(length(state_counties))) {
    
    HPI_na <- sum(is.na(county_df$HPI))
    
    nrow_county <- sum(df_import$fips==state_counties[[j]])
    
    if(HPI_na==nrow_county) {
      ### Subset to current county ####
      
      county_df <- df_import %>%
        dplyr::filter(fips==state_counties[[j]]) %>%
        dplyr::select(!HPI)
      
    } else {
      
      county_df <- df_import %>%
        dplyr::filter(fips==state_counties[[j]])
      
    }
    
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
      ### Regression ####
      
      baseMod <- lm(log_priceadj_ha ~ ., data = model_df %>%
                      dplyr::select(!c(sid, fips)) %>%
                      na.omit())
      
      ### Collect Performance Stats ####
      
      #### Mean Sq. Error ####
      mse_base <- c(crossprod(baseMod$residuals))/length(baseMod$residuals)
      
      #### Percent obs from neighbors ####
      n_neighbors <- model_df %>%
        filter(fips != state_counties[[j]]) %>%
        nrow()
      
      percent_neighbors <- n_neighbors/nrow(model_df)
      
      county_stats_base <- glance(baseMod) %>%
        mutate(mse = mse_base,
               fips = state_counties[[j]],
               percent_neighbors = percent_neighbors)
      
      collect_stats_base <- rbind(collect_stats_base, county_stats_base)
      
      
      ### Collect Predictions ####
      
      county_pred_reg <- tibble(.pred = baseMod$fitted.values,
             log_priceadj_ha = baseMod$model[,c('log_priceadj_ha')],
             fips = state_counties[[j]])

      collect_pred_reg <- rbind(collect_pred_reg, county_pred_reg)
      
        
      ### Variable Importance ####
      county_import_reg <- broom::tidy(baseMod) %>%
        dplyr::select(term, estimate, std.error) %>%
        filter(term != "(Intercept)") %>%
        mutate(fips = state_counties[[j]]) %>%
        pivot_wider(
          names_from = term,
          values_from = c(estimate, std.error),
          names_sep = "!!"
        )

      collect_import_reg <- rbind(collect_import_reg, county_import_reg)
      
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
  
  setwd('/home/rstudio/users/gold1/fmv/data/model/reg')
  
  ### Performance Stats ####
  write_parquet(collect_stats_base, 
                paste0("performance/base/stats_",state, ".pqt"))
  
  ### Predictions ####
  write_parquet(collect_pred_reg, 
                paste0('predictions/pred_', state, ".pqt"))
  
  ### Importance ####
  write_parquet(collect_import_reg, 
                paste0('importance/import_', state, ".pqt"))
  
  cat("\n\nFinished: ", state, "\n\n", sep = "")
  
  end <- Sys.time()
  
}