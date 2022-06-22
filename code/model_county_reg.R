library(MASS)
library(broom)
library(patchwork)
library(tidyverse)
library(magrittr)

state_counties <- 
  get(df_final) %>%
  dplyr::pull(fips) %>%
  unique()

# Specify model stats df to be grown later ####
collect_stats_base <- 
  tibble::tibble(stat = c("r.squared", "adj.r.squared", "sigma",
                  "statistic", "p.value", "df", "logLik",
                  "AIC", "BIC", "deviance", "df.residual",
                  "nobs", "rmse", "fips", "percent_neighbor")) %>% 
  dplyr::mutate(na = NA) %>%
  pivot_wider(
    names_from = stat,
    values_from = na) %>%
  slice(0)

collect_stats_aic <- 
  tibble(stat = c("r.squared", "adj.r.squared", "sigma",
                  "statistic", "p.value", "df", "logLik",
                  "AIC", "BIC", "deviance", "df.residual",
                  "nobs", "rmse", "fips", "percent_neighbor")) %>% 
  mutate(na = NA) %>%
  pivot_wider(
    names_from = stat,
    values_from = na) %>%
  slice(0)


# Loop through all counties ####

for (j in seq_len(length(state_counties))) {
  
  neighbors <- county_adjacency %>%
    dplyr::filter(countyname != neighborname) %>%  
    dplyr::filter(fipscounty == state_counties[[j]]) %>%
    pull(fipsneighbor)
  
  county_df <- get(df_final) %>%
    dplyr::select(log_priceadj_ha, fips, any_of(nolte2020vars)) %>% # select model vars
    dplyr::filter(fips==state_counties[[j]]) 
  
  
  if (nrow(county_df) < 1000) {
    
    rows_needed <- 1000 - nrow(county_df)
    
    neighbor_df <- get(df_final) %>%
      dplyr::select(log_priceadj_ha, fips, any_of(nolte2020vars)) %>%
      filter(fips %in% neighbors)
    
    if(rows_needed <= nrow(neighbor_df)) {
      
      model_df <- bind_rows(county_df,
                            neighbor_df %>% slice_sample(n = rows_needed))
      
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
                      dplyr::select(!fips) %>%
                      na.omit())
      
      stepMod <- stepAIC(lm(log_priceadj_ha ~ ., 
                            data = model_df %>%
                              na.omit() %>%
                              dplyr::select(!fips)), 
                         trace = FALSE, 
                         direction = "both")
      
      # aic_vars <- names(stepMod$model)
      
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

collect_stats_all[[state]] <- collect_stats_base
