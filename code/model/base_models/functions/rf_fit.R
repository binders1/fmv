
rf_fit <- function(county, ...) {

  county_data <-
    state_data %>%
    filter(fips == county)
  
  HPI_na <- sum(is.na(state_data$HPI))
  nrow_county <- sum(state_data$fips==county)
  if (HPI_na >= nrow_county) county_data %<>% select(!HPI)
  
  # Check that county has 1000 obs; if not, take neighbor donations ===========
  pre_prep_data <-
    county_check_nrow(county_data, ...)
  
  # If even neighbor donations couldn't get county to 1000, exit
  if (nrow(pre_prep_data) < 1000) return()
  # Otherwise, prep data for modeling =========================================
  
  # Remove fips variable (isn't modeled) and all rows with NA values
  rf_data <-
    pre_prep_data %>%
    select(!fips) %>%
    na.omit()
    
  # Set seed to ensure replicability
  set.seed(2366132)
    
  # Split data into training and test sets, stratified on sale price
  rf_split <- initial_split(rf_data, strata = log_priceadj_ha)
  train <- training(rf_split)
  test <- testing(rf_split)
    
  # Model specification =======================================================
    
  # Build formula and give sales id a non-predictor role as id variable
  rf_recipe <-
    recipe(formula = log_priceadj_ha ~ .,
           data = train) %>%
    update_role(sid, new_role = "id")
  
  # Specify model, parameters, mode, model engine, and model rules
  rf_model_spec <- 
    rand_forest(mtry = length(names(train))/3,
                min_n = 3,
                trees = 500) %>%
    set_mode("regression") %>%
    set_engine(
      "ranger", 
      splitrule = "extratrees",
      importance = "permutation"
    )
  
  # Join formula and model specification to create a workflow object,
  # which can be passed to fitting functions
  rf_workflow <-
    workflow(rf_recipe, rf_model_spec)
  
  # Fit and predict manually to allow for predictions on ALL sids =============
  
  # ------------------------------------------------------------------------- #
  # NB: this manual process and last_fit() produce nearly identical predictions
  # ------------------------------------------------------------------------- #
  
  # Fit model using training data
  rf_train_fit <- fit(rf_workflow, train)
  
  # Predict ALL parcels (training + test)
  predict_all <- 
    predict(rf_train_fit, rf_data) %>%
    bind_cols(
      rf_data %>% select(sid), .
    )
  
  # Use tidymodels built-in model evalution to train -> test ==================
  rf_last_fit <- last_fit(rf_workflow, rf_split)
  
  # Record sample size from county and neighbors
  county_stats <-
    tibble(
      fips = county,
      n_obs = nrow(rf_data),
      n_county = pre_prep_data %>% filter(fips == county) %>% nrow(),
      n_neighbor = pre_prep_data %>% filter(fips != county) %>% nrow()
      )
  
  # Return the following:
  list(
    
    # 1. Predictions for all sales records
    "predict_all" = predict_all,
    
    # 2. Model evaluation object (including metrics and predictions)
    "rf_last_list" = rf_last_fit,
    
    # 3. n observations in model, observations in county and neighbor
    "county_stats" = county_stats
  )
}


# Checks county has >= 1000 obs; if not, attempts padding with neighbors ======
county_check_nrow <- function(county_data, ...) {
  
  county_nrow <- nrow(county_data)
  county <- unique(county_data$fips)
  
  county_data <-
    county_data %>%
    select(any_of(...))
  
  model_data <-
    # If county has sufficient observations...
    if (county_nrow >= 1000) {
      # ...return as-is
      county_data %>%
        select(any_of(...))
      } else {
        # If county has less than 1000 observations, attempt to 
        # reach 1000 observations by taking from neighbors
        neighbor_donate(county, county_data) %>%
          select(any_of(...))
      }
  
  model_data
  }


# Attempts to pad county data to 1000 observations with neighbors' data =======
neighbor_donate <- function(county, county_data) {
  
  rows_needed <- 1000 - county_nrow
  
  # ...create dataset of observations in neighboring counties
  neighbor_data <-
    find_neighbors(county) %>%
    dplyr::select(dplyr::any_of(names(county_data)))
  
  # If neighboring counties can provide all needed rows...
  if (nrow(neighbor_data) >= rows_needed) {
    
    # 1) sample neighbor data down to only rows needed to reach 1000 obs
    neighbor_rows_needed <-
      neighbor_data %>%
      slice_sample(n = rows_needed)
    
    # 2) bind neighbor sample to focal county data
    bind_rows(
      county_data,
      neighbor_data
    )
  } else {
    # If neighbors cannot provide all needed rows, return as-is
    county_data
    }
  }


# Filters state dataset to only neighbors of specified county ================
find_neighbors <- function(county) {
  
  neighbors <- 
    county_adjacency %>% 
    dplyr::filter(fipscounty == county) %>%
    dplyr::pull(fipsneighbor)

  state_data %>%
    dplyr::filter(fips %in% neighbors)
}



