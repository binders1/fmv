# Load Packages ####
# install.packages(c('tidymodels','usemodels','vip','ranger'))
library(tidymodels)
library(usemodels)
library(vip)
library(doParallel)
library(showtext)
library(magrittr)

tidymodels_prefer()


# Specify current state counties ####
state_counties <- 
  get(df_final) %>%
  pull(fips) %>%
  unique()

# Specify model stats df to be grown later ####
collect_stats_rf <- 
  tibble(stat = c("rmse", "rsq", "mse",
                  "fips", "nobs", "percent_neighbor")) %>% 
  mutate(na = NA) %>%
  pivot_wider(
    names_from = stat,
    values_from = na) %>%
  slice(0)



# Loop through all counties ####

for (j in seq_len(length(state_counties))) {
  
  neighbors <- county_adjacency |>
    dplyr::filter(countyname != neighborname) |>  
    dplyr::filter(fipscounty == state_counties[[j]]) |>
    dplyr::pull(fipsneighbor)
  
  county_df <- get(df_final) |>
    dplyr::select(log_priceadj_ha, fips, any_of(nolte2020vars)) |> # select model vars
    dplyr::filter(fips==state_counties[[j]]) 
  
  
  if (nrow(county_df) < 1000) {
    
    rows_needed <- 1000 - nrow(county_df)
    
    neighbor_df <- get(df_final) |>
      dplyr::select(log_priceadj_ha, fips, any_of(nolte2020vars)) |>
      dplyr::filter(fips %in% neighbors)
    
    if(rows_needed <= nrow(neighbor_df)) {
      
      model_df <- dplyr::bind_rows(county_df,
                                   neighbor_df |> 
                                     slice_sample(n = rows_needed))
      
    } else {
      
      model_df <- county_df
      
    } 
  } else {
    model_df <- county_df
  }
  
  
  
  if(nrow(model_df)>=1000) {
    
    ## Modeling ####
    
    ### clean data ####
    rf_data <- model_df |>
      dplyr::select(!fips) |>
      stats::na.omit()
    
    
    ### Construct Model ####
    
    # split data
    set.seed(319)
    rf_split <- rsample::initial_split(rf_data, strata = log_priceadj_ha)
    train <- rsample::training(rf_split)
    test <- rsample::testing(rf_split)
    
    
    # build resamples for cross-validation during tuning
    set.seed(194)
    
    rf_folds <- rsample::vfold_cv(train, strata = log_priceadj_ha) # how many folds should we do?
    
    
    ### Model Workflow ####
    
    # specify general model formula
    ranger_recipe <- 
      recipes::recipe(formula = log_priceadj_ha ~ ., data = train)
    # preprocessing goes here, if relevant;
    # see https://recipes.tidymodels.org/reference/ for more on preprocessing functions
    
    # specify modelling engine and mode; 
    # tune() is inserted as a placeholder for the hyperparameter tuning later
    ranger_spec <- 
      parsnip::rand_forest(mtry = tune(), min_n = tune(), trees = 500) |> 
      set_mode("regression") |> 
      set_engine("ranger")
    # do I need to specify importance method (e.g., impurity) here?
    
    
    # specify workflow: model, preprocessing (if relevant), etc.
    ranger_workflow <- 
      workflow() |> 
      add_recipe(ranger_recipe) |> # the recipe in the formula
      add_model(ranger_spec)  # the model are the parameters, engine, importance methods, etc.
    
    
    
    #### Hyperparameter Tuning ####
    
    set.seed(15224)
    if(getDoParWorkers()<64) {
      
      cl <- makeCluster(64)
      registerDoParallel(cl)
      
    }
    
    
    # hyperparameter tuning; 
    # it takes a while to go through all bootstrapped cross-validation sets
    system.time(
      ranger_tune <- tune_grid(ranger_workflow, 
                               resamples = rf_folds,
                               grid = 11)
    )
    
    
    
    
    # construct a final random forest model with the best hyperparameters from the 
    # kfold cross-validation grid-search tuning above
    final_rf <- ranger_workflow |>
      finalize_workflow(select_best(ranger_tune, 
                                    metric = "rsq"))
    
    
    ### Fitting ####
    
    rf_fit <- last_fit(final_rf, rf_split)
    
    n_obs <- nrow(rf_data)
    
    n_neighbors_rf <- model_df |>
      dplyr::filter(fips != state_counties[[j]]) |>
      nrow()
    
    county_stats_rf <- collect_metrics(rf_fit) |>
      select(.metric, .estimate) |>
      spread(.metric, .estimate) |>
      mutate(
        mse = rmse^2,
        fips = state_counties[[j]],
        nobs = n_obs,
        percent_neighbor = n_neighbors_rf/nobs) 
    
    
    collect_stats_rf <- collect_stats_rf |>
      rbind(county_stats_rf)
    
    cat("Complete: ", state_counties[[j]], 
        " |.....| Modeled: YES, n.obs: ", nrow(model_df), 
        " |.....| % Neighbors: ", 
        county_stats_rf$percent_neighbor, "\n",
        sep = "")
    
  } else {
    
    cat("Complete: ", state_counties[[j]], 
        " |.....| Modeled:  NO, n.obs: ", 
        nrow(county_df)+nrow(neighbor_df), "\n",
        sep = "")
    
  }
  
}

