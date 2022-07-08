library(MASS)
library(broom)
library(patchwork)
library(tidyverse)
library(magrittr)
library(arrow)



setwd("/home/rstudio/users/gold1/fmv/data/cleaned")

all_clean <- list.files()


state <- str_extract(all_clean[[i]], "[:upper:]{2}")

df_import <- read_parquet(all_clean[[i]]) %>%
  dplyr::select(!HPI)

state_counties <- df_import %>%
  dplyr::pull(fips) %>%
  unique()

# Load County Adjacency df ####
county_adjacency <- 
  readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv")


# Specify model stats df to be grown later ####
collect_stats_base <- 
  tibble::tibble(r.squared = NA, adj.r.squared = NA, sigma = NA,
                  statistic = NA, p.value = NA, df = NA, logLik = NA,
                  AIC = NA, BIC = NA, deviance = NA, df.residual = NA,
                  nobs = NA, rmse = NA, fips = NA, percent_neighbor = NA) %>% 
  slice(0)

collect_stats_aic <- collect_stats_base


# Specify empty df for AIC vars ####
aic_var_tbl <- tibble(rowid = seq_len(114))



# Loop through all counties ####
cl <- parallel::makeCluster(64)
doParallel::registerDoParallel(cl)
foreach::getDoParWorkers()

for(j in seq_len(length(state_counties))) {
  
  neighbors <- county_adjacency %>%
    dplyr::filter(countyname != neighborname) %>%  
    dplyr::filter(fipscounty == state_counties[[j]]) %>%
    pull(fipsneighbor)
  
  county_df <- df_import %>%
    dplyr::filter(fips==state_counties[[j]]) 
  
  if (nrow(county_df) < 1000) {
    
    rows_needed <- 1000 - nrow(county_df)
    
    neighbor_df <- df_import %>%
      filter(fips %in% neighbors)
    
    if(rows_needed <= nrow(neighbor_df)) {
      
      model_df <- bind_rows(county_df,
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
      
     stepMod <- stepAIC(baseMod, 
                         trace = FALSE, 
                         direction = "backward")
      
      aic_vars <- names(stepMod$model) %>%
        tibble() %>%
        rename(!!state_counties[[j]] := 1) %>%
        rowid_to_column()
      
      aic_var_tbl <- aic_var_tbl %>%
        left_join(aic_vars)
      
      mse_base <- c(crossprod(baseMod$residuals))/length(baseMod$residuals)
      
      mse_aic <- c(crossprod(stepMod$residuals))/length(stepMod$residuals)
      
      
      #### Percent obs from neighbors ####
      n_neighbors <- model_df %>%
        filter(fips != state_counties[[j]]) %>%
        nrow()
      
      percent_neighbors <- n_neighbors/nrow(model_df)
      
      
      #### Collect county stats #### 
      county_stats_base <- glance(baseMod) %>%
        mutate(mse = mse_base,
               fips = state_counties[[j]],
               percent_neighbors = percent_neighbors)
      
      county_stats_aic <- glance(stepMod) %>%
        mutate(mse = mse_aic,
               fips = state_counties[[j]],
               percent_neighbors = percent_neighbors)
      
      
      
      #### Bind current county stats to stats df ####
      collect_stats_base <- collect_stats_base %>%
        rbind(county_stats_base)
      
      collect_stats_aic <- collect_stats_aic %>%
        rbind(county_stats_aic)
      
      cat("Complete: ", state_counties[[j]], 
          " |.....| Modeled: YES, n.obs: ", nrow(model_df), 
          " |.....| % Neighbors: ", percent_neighbors, "\n",
          sep = "")
      
    } else {
    
    cat("Complete: ", state_counties[[j]], 
        " |.....| Modeled:  NO, n.obs: ", 
        nrow(county_df)+nrow(neighbor_df), "\n",
        sep = "")
  
    }
  
}

collect_stats_base_all[[state]] <- collect_stats_base



