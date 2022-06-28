library(tidymodels)

setwd("/home/rstudio/users/gold1/fmv/data/cleaned")

df_nation <- map_dfr(list.files(),
        read_parquet) %>%
  slice_sample(n = 200000)

model_df <- df_nation %>%
  dplyr::select(!c(sid, fips, HPI, cst_2500, cst_50)) %>%
  mutate(across(starts_with("VALUE"), ~ replace_na(.x, 0))) %>%
  stats::na.omit()


# Model ####

## Construct Model ####

# split data
set.seed(319)
rf_split <- rsample::initial_split(model_df, strata = log_priceadj_ha)
train <- rsample::training(rf_split)
test <- rsample::testing(rf_split)


## Model Workflow ####

### Formula and Preprocessing ####
ranger_recipe <- 
  recipes::recipe(formula = log_priceadj_ha ~ ., 
                  data = train)

### Engine, Mode, Method ####
ranger_spec <-
  parsnip::rand_forest(mtry = length(names(train))/3, 
                       min_n = 3, # increase required sample leaf size to avoid overfitting
                       trees = 500) %>% # try 250 trees
  set_mode("regression") %>%
  set_engine("ranger",
             splitrule = "extratrees",
             importance = "permutation")


### Build Workflow ####
ranger_workflow <- workflow() %>% 
  add_recipe(ranger_recipe) %>% # the recipe in the formula
  add_model(ranger_spec) # the parameters, engine, importance methods, etc.



## Model Fitting ####

if(foreach::getDoParWorkers()<64) {
  
  doParallel::registerDoParallel(64)
  
}

system.time(
  
  rf_fit <- last_fit(ranger_workflow, rf_split)
  
)

## Collect Stats ####

### Collect Performance Stats ####

n_obs <- nrow(model_df)

collect_metrics(rf_fit) %>%
  select(.metric, .estimate) %>%
  spread(.metric, .estimate) %>%
  mutate(
    mse = rmse^2,
    frr = k,
    nobs = n_obs) 



### Collect Predictions ####

frr_predictions <- collect_predictions(rf_fit) %>%
  mutate(frr = k)


### Variable Importance ####

importance <- ranger_workflow %>%
  fit(test) %>%
  extract_fit_parsnip() %>%
  vip::vi() %>%
  add_row(Variable = "frr",
          Importance = k) %>%
  pivot_wider(
    names_from = Variable,
    values_from = Importance
  ) %>%
  relocate(frr)

## Write stats ####
