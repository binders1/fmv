# FUNC: given a county dataframe, fits an ERT model ####

# Purpose: low-level nested function within county ERT models. 
# See ~/fmv/code/model/model_county_rf for use case

# Args ####
## j: integer value of county, indexed within the state_counties vector 


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