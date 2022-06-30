# Load Packages ####
library(tidyverse)
library(googlesheets4)

# Load custom functions ####

source("~/fmv/code/custom_functions.R")


# Load df of observations in FRR i ####

## State abbr-to-number ref table ####
state_ref_tbl <-read_csv('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv',
                         show_col_types = F)

## Build FRR-county reference table ####
ss <- as_sheets_id("https://docs.google.com/spreadsheets/d/1rUfzSfVXLjYnI6hlO-WWR588hKI3NCMiPYHHc1JR2zs/edit#gid=1580896317")

ag_regions <-read_sheet(ss = ss, skip = 2)

ag_regions_key <- ag_regions %>%
  dplyr::select(7) %>%
  slice(1:9) %>%
  separate(col = 1,
           into = c('id','name'),
           sep = "=") %>%
  mutate(id = as.double(id))


ag_regions_ref <- ag_regions %>%
  dplyr::select(1:2) %>%
  rename(fips = "Fips", id = 2) %>%
  mutate(fips = ifelse(nchar(fips)==4, 
                       paste0("0",fips),
                       fips),
         state = str_sub(fips, 1, 2)) %>%
  left_join(ag_regions_key) %>%
  left_join(state_ref_tbl, by = c('state' = 'st'))

## Specify states to load in ####
states_to_load <- ag_regions_ref %>%
  filter(id == k) %>%
  pull(stusps) %>%
  unique()

counties_to_include <- ag_regions_ref %>%
  filter(id == k) %>%
  pull(fips) %>%
  sort()

clean_to_load <- paste0("clean_", states_to_load, ".pqt") %>%
  sort()
  

## Import current FRR dataframe ####
setwd("~/fmv/data/cleaned")

df_import <- map_dfr(clean_to_load[1], ~ read_parquet(.x)) %>% 
  filter(fips %in% counties_to_include)


model_df <- df_import %>%
  select(!c(sid, fips, HPI)) %>%
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

unregister()

if(getDoParWorkers()<64) {
  
  registerDoParallel(64)
  
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




