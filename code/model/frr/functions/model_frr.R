
model_frr <- function(model_df) {
  
  # Model ####
  
  cli::cli_h2("Constructing Model")
  
  ## Construct Model ####
  
  # split data
  set.seed(319)
  rf_split <- rsample::initial_split(model_df, 
                                     strata = log_priceadj_ha)
  train <- rsample::training(rf_split)
  test <- rsample::testing(rf_split)
  
  
  ## Model Workflow ####
  
  ### Formula and Preprocessing ####
  ranger_recipe <- 
    recipes::recipe(formula = log_priceadj_ha ~ ., 
                    data = train) %>%
    update_role(sid, new_role = "id variable")
  
  ### Engine, Mode, Method ####
  ranger_spec <-
    parsnip::rand_forest(mtry = length(names(train))/3, 
                         min_n = 3, # increase leaf size to avoid overfitting
                         trees = 500) %>% # try 250 trees
    set_mode("regression") %>%
    set_engine("ranger",
               splitrule = "extratrees",
               importance = "permutation")
  
  
  ### Build Workflow ####
  ranger_workflow <- 
    workflow() %>% 
    add_recipe(ranger_recipe) %>% # the recipe in the formula
    add_model(ranger_spec) # the parameters, engine, importance methods, etc.
  
  
  
  ## Model Fitting ####
  
  cli::cli_h2("Model Fitting")
  
  tic('Fitting RF model')
  
  rf_fit <- last_fit(ranger_workflow, rf_split)
  
  toc()
  
  
  
}