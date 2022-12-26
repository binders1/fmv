model_everything <- 
  function(
    .data, 
    frr_id, 
    buildings, 
    axe = TRUE
    ) {

    # Model ####
    if (!is.null(.data)) {
      cli::cli_h2("Constructing Model")
    } else {
      cli::cli_abort("No data passed to predict_everything()")
    }
    
    ## Construct Model ####
    
    # split data
    set.seed(319)
    rf_split <- rsample::initial_split(.data, strata = log_priceadj_ha)
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
    fitted_frr <- 
      workflow() %>% 
      add_recipe(ranger_recipe) %>% # the recipe in the formula
      add_model(ranger_spec) %>% # the parameters, engine, importance methods, etc.
      fit(train)
    
    # Reduce size of fitted model object
    if (axe) {
      fitted_frr <-
        fitted_frr %>% 
        butcher::axe_data() %>%
        butcher::axe_fitted()
    }
    
    fitted_frr

    }
