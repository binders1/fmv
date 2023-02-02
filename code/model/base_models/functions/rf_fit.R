# rf_fit() ==================================================================
#
# Given a county dataframe, fits an ERT model
# Low-level nested function within county ERT models. 
#
## j: integer value of county, indexed within the state_counties vector 
## ...: variables to be included for modeling

rf_fit <- function(county, ...) {
  
  HPI_na <- sum(is.na(df_import$HPI))
  
  nrow_county <- sum(df_import$fips==county)
  
  if (HPI_na >= nrow_county) {
    
    ### Subset to current county ####
    
    county_df <- df_import %>%
      dplyr::filter(fips==county) %>%
      select(!HPI)
    
  } else {
    
    county_df <- df_import %>%
      dplyr::filter(fips==county)
    
  }
  
  ### Vector of neighbors ####
  neighbor_vec <- 
    county_adjacency %>%
    dplyr::filter(countyname != neighborname) %>%  
    dplyr::filter(fipscounty == county) %>%
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
        dplyr::select(...) %>%
        stats::na.omit()
      
      mod_nrow <- dplyr::bind_rows(county_df,
                                        neighbor_df %>% 
                                          slice_sample(n = rows_needed)) %>%
                         dplyr::select(...) %>%
        nrow()
      
    } else {
      
      model_df <- county_df %>%
        dplyr::select(...) %>%
        stats::na.omit()
      
      mod_nrow <- nrow(
        county_df %>%
          dplyr::select(...)
      )
      
    } 
    
  } else {
    
    model_df <- county_df %>%
      dplyr::select(...) %>%
      stats::na.omit()
    
    
    mod_nrow <- 
      nrow(
        county_df %>%
          dplyr::select(...)
        )
    
  }
  
  # Don't model if neighbors couldn't get county to 1000 obs
  if(mod_nrow < 1000) return()
    
    ## Modeling ####
    
    rf_data <- model_df %>%
      dplyr::select(!fips) 
    
    
    ### Construct Model ####
    
    # split data
    set.seed(319)
    
    tryCatch(
      
      rf_split <- rsample::initial_split(rf_data, 
                                         strata = log_priceadj_ha),
      
      error = function(e)
        message('Model split error in: ',county)
      
    )
    
    
    train <- rsample::training(rf_split)
    test <- rsample::testing(rf_split)
    
    ### Model Workflow ####
    
    #### Formula and Preprocessing ####
    ranger_recipe <- 
      recipes::recipe(formula = log_priceadj_ha ~ ., 
                      data = train) %>%
      recipes::update_role(sid, new_role = "id")
    
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
      rf_fit <- 
        tune::last_fit(ranger_workflow, rf_split) %>%
        dplyr::mutate(
          fips = county,
          n_obs = nrow(rf_data),
          n_county = nrow(model_df %>% dplyr::filter(fips == county)),
                      n_neighbor = nrow(model_df %>% 
                                          dplyr::filter(fips != county))),
      
      error = function(e)
        warning('Model fit error in county ', county)
    )
      
    # Return testing set and model fit
    list(
        "test_set" = test,
        "rf_fit" = rf_fit
      )
  
}