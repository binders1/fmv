
# Generic function that dispatches to county and frr methods ==================
rf_fit <- function(geo = c("county", "frr"), ...) {
  geo <- match.arg(geo)
  
  fit_fn <- switch(geo,
                   county = rf_fit.county,
                   frr = rf_fit.frr)
  fit_fn(...)
}

# FRR method ==================================================================
rf_fit.frr <- function(frr, model_data) {
  
  # Split data ================================================================
  set.seed(60615)
  rf_split <- rsample::initial_split(model_data, strata = log_priceadj_ha)
  train <- rsample::training(rf_split)
  test <- rsample::testing(rf_split)

  # Model Workflow ============================================================
  # Formula
  rf_recipe <- 
    recipes::recipe(formula = log_priceadj_ha ~ ., data = train) %>%
    update_role(sid, new_role = "id variable")
  
  # Engine, Mode, Method
  rf_spec <-
    parsnip::rand_forest(mtry = length(names(train))/3, 
                         min_n = 3,
                         trees = 500) %>%
    set_mode("regression") %>%
    set_engine("ranger",
               splitrule = "extratrees",
               importance = "permutation")
  
  # Workflow object
  rf_workflow <- workflow(rf_recipe, rf_spec)
  
  # Fit model =================================================================
  
  # Fit model using training data
  rf_train_fit <- fit(rf_workflow, train)
  
  # Predict ALL parcels (training + test)
  predict_all <- 
    predict(rf_train_fit, model_data) %>%
    bind_cols(model_data[c('sid', 'log_priceadj_ha')], .)
  
  # Use tidymodels built-in model evalution to train -> test ==================
  rf_last_fit <- last_fit(rf_workflow, rf_split)
  
  # Bind sale record IDs to predictions so they can be identified later
  rf_last_fit$.predictions[[1]] <-
    bind_sid_to_pred(
      .pred = rf_last_fit$.predictions[[1]],
      test_set = test)
  
  # Collect FRR-level sample-size statistics
  frr_stats <- 
    tibble(
      frr = frr,
      n_obs = nrow(data),
      n_train = nrow(train),
      n_test = nrow(test)
    )
    
  # Return the following:
  list(
    
    # 1. Predictions for all sales records in FRR
    "predict_all" = predict_all,
    
    # 2. Model evaluation object (including metrics and predictions)
    "rf_last_fit" = rf_last_fit,
    
    # 3. n observations in model, observations in county and neighbor
    "frr_stats" = frr_stats
  )
  
  }






# County method ===============================================================
rf_fit.county <- function(county_data, neighbor_data, ...) {
  
  # Remove HPI column in counties with no HPI data ============================
  no_HPI_data <- all(is.na(county_data$HPI))
  if (no_HPI_data) county_data <- county_data %>% select(!HPI)
  
  # Check that county has 1000 obs; if not, take neighbor donations ===========
  pre_prep_data <-
    county_check_nrow(county_data, neighbor_data, ...)
  
  # If even neighbor donations couldn't get county to 1000, exit
  if (nrow(pre_prep_data) < 1000) return()
  # Otherwise, prep data for modeling =========================================
  
  # Remove fips variable (it's not modeled) and all rows with NA values
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
  # NB: this manual process and last_fit() produce **mostly** similar pred values
  # ------------------------------------------------------------------------- #
  
  # Fit model using training data
  rf_train_fit <- fit(rf_workflow, train)
  
  # Predict ALL parcels (training + test)
  predict_all <- 
    predict(rf_train_fit, rf_data) %>%
    bind_cols(
      rf_data[c('sid', 'log_priceadj_ha')], .
    )
  
  # Use tidymodels built-in model evalution to train -> test ==================
  rf_last_fit <- last_fit(rf_workflow, rf_split)
  
  # Bind sale record IDs to predictions so they can be identified later
  rf_last_fit$.predictions[[1]] <-
    bind_sid_to_pred(
      .pred = rf_last_fit$.predictions[[1]],
      test_set = test)
  
  
  # Record sample size from county and neighbors
  county <- unique(county_data$fips)
  county_stats <-
    tibble(
      fips = county,
      
      n_obs = nrow(rf_data),
      n_train = nrow(train),
      n_test = n_obs - n_train,
      
      n_county = pre_prep_data %>% filter(fips == county) %>% nrow(),
      n_neighbor = pre_prep_data %>% filter(fips != county) %>% nrow()
      )
  
  # Return the following:
  list(
    
    # 1. Predictions for all sales records
    "predict_all" = predict_all,
    
    # 2. Model evaluation object (including metrics and predictions)
    "rf_last_fit" = rf_last_fit,
    
    # 3. n observations in model, observations in county and neighbor
    "county_stats" = county_stats
  )
}

# ============================================================================ #
# Helper functions                                                        ====
# ============================================================================ #

# Checks county has >= 1000 obs; if not, attempts padding with neighbors ======
county_check_nrow <- function(county_data, neighbor_data, ...) {
  
  county_nrow <- nrow(county_data)
  county <- unique(county_data$fips)
  
  model_data <-
    # If county has sufficient observations...
    if (county_nrow >= 1000) {
      # ...return as-is
      county_data %>%
        select(any_of(...))
      } else {
        # If county has less than 1000 observations, attempt to 
        # reach 1000 observations by taking from neighbors
        neighbor_donate(county_data, neighbor_data) %>%
          select(any_of(...))
      }
  
  model_data
  
  }


# Attempts to pad county data to 1000 observations with neighbors' data =======
neighbor_donate <- function(county_data, neighbor_data) {
  
  rows_needed <- 1000 - nrow(county_data)
  stopifnot(rows_needed >= 0)
  
  # Dataset of observations in neighboring counties
  neighbor_data <- 
    neighbor_data %>%
    dplyr::select(dplyr::any_of(names(county_data)))
  
  # If neighbors cannot provide all needed rows, return county as-is
  if (nrow(neighbor_data) < rows_needed) return(county_data)
  
  # Otherwise, donate neighbor data ---
  
  # 1) sample neighbor data down to only rows needed to reach 1000 obs
  neighbor_rows_needed <-
    neighbor_data %>%
    slice_sample(n = rows_needed)
    
    # 2) bind neighbor sample to focal county data
    bind_rows(
      county_data,
      neighbor_rows_needed
    )
    
    }


# Bind sale record IDs to rf_fit test set predicted values ====================
bind_sid_to_pred <- function(.pred, test_set) {
  
  test_with_rowid <-
    test_set %>%
    rowid_to_column(var = ".row") %>%
    select(.row, sid)
  
  .pred %>%
    left_join(test_with_rowid, by = ".row")
  
}
