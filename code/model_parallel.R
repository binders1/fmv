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

countyRF <- function(i) {

  
  source('/home/rstudio/users/gold1/fmv/code/custom_functions.R')

  ## Load County Adjacency df ####
  county_adjacency <- readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
                                      show_col_types = F)
  
  ## Set working directory ####
  setwd("/home/rstudio/users/gold1/fmv/data/cleaned")
  # setwd("Y://data/cleaned")
  
  ## Vector of all state pqt files ####
  all_clean <- list.files()
  
  
    # (re)set working directory to clean data folder
    setwd("/home/rstudio/users/gold1/fmv/data/cleaned")
  
    ## Current state ####
    state <- stringr::str_extract(all_clean[[i]], "[:upper:]{2}")
    
    ## Import current state df ####
    df_import <- arrow::read_parquet(all_clean[[i]])
    
    
    ## Specify current state counties ####
    state_counties <- df_import %>%
      pull(fips) %>%
      unique()
    
    
    ## county ####
    
    
    
    
    
    state_rf_fit <- foreach::foreach(j=seq_len(length(state_counties))) %dopar% {
      
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
          dplyr::filter(fips %in% neighbor_vec)
        
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
        rf_split <- rsample::initial_split(rf_data, 
                                           strata = log_priceadj_ha)
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
          set_mode("regression") %>%
          set_engine("ranger",
                     splitrule = "extratrees",
                     importance = "permutation")
        
        #### Build Workflow ####
        ranger_workflow <- 
          workflow() %>% 
          add_recipe(ranger_recipe) %>% # the recipe in the formula
          add_model(ranger_spec) # the parameters, engine, importance methods, etc.
        
        ### Model Fitting ####
        rf_fit <- last_fit(ranger_workflow, rf_split) %>%
          mutate(fips = county_df$fips[[1]])
        
      }
      
      rf_fit
      
    } %>%
      bind_rows()
    
   
    setwd("/home/rstudio/users/gold1/fmv/data/model/county/rf")
    ## Write state-level model stats to file ####
    
    write_parquet(state_predictions, 
                  paste0("predictions/pred_",state, ".pqt"))
    
    write_parquet(state_stats, 
                  paste0('performance/stats_', state, ".pqt"))
    
    write_parquet(state_importance, 
                  paste0('importance/import_', state, ".pqt"))
    
    }

















if(foreach::getDoParWorkers()<64) {
  
  doParallel::registerDoParallel(64)
  
}


done <- list.files('/home/rstudio/users/gold1/fmv/data/model/county/rf/performance') %>%
  str_extract("[:upper:]{2}")

to_do <- list.files('/home/rstudio/users/gold1/fmv/data/cleaned') %>%
  sort() %>%
  tibble(pqt = .) %>%
  mutate(state = str_extract(pqt, "[:upper:]{2}")) %>%
  rowid_to_column() %>%
  filter(!is.element(state,done)) %>%
  pull(rowid)

foreach::foreach(i=to_do) %dopar% {
    
    countyRF(i)
  
}


