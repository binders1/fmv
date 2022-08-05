# Set-up ####

## Load Packages ####
library(tidyverse)
library(googlesheets4)
library(tictoc)
library(cli)
library(arrow)
library(tidymodels)

tidymodels_prefer()



## Set dirs ####

root <- "~/fmv"

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
odir <- file.path(root, "output")

f_dir <- file.path(cdir, "functions")
m_dir <- file.path(cdir, "model")

clean_dir <- file.path(ddir, "cleaned")


## Authorize gs4 connection ####
gs4_auth("gold1@stolaf.edu")

## Load custom functions ####

fdir <- "~/fmv/code/functions"

walk(list.files(fdir),
     ~ source(file.path(fdir, .x))
     )

## Load Nolte (2020) features
noltevars_path <- file.path(ddir, "nolte2020vars.csv")

nolte2020vars <- 
  read_csv(noltevars_path,
           show_col_types = F) %>% 
  pull()

### Add 45-deg rotated spatial vars ####
nolte2020vars <-
  c(nolte2020vars, "x45", "y45")

## Load Nolte Counties ####
nolte_counties <- 
  read_csv(file.path(ddir, "nolte_counties.csv"),
           show_col_types = F) %>%
  pull(fips)

## State abbr-to-number ref table ####
state_ref_tbl <-
  read_csv('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv',
           show_col_types = F)

## Build FRR-county reference table ####
ss <- as_sheets_id("1rUfzSfVXLjYnI6hlO-WWR588hKI3NCMiPYHHc1JR2zs")

ag_regions <- read_sheet(ss = ss, skip = 2)

ag_regions_key <- ag_regions %>%
  dplyr::select(7) %>%
  slice(1:9) %>%
  separate(col = 1,
           into = c('id','frr_name'),
           sep = "=") %>%
  mutate(id = as.double(id))


ag_regions_ref <- ag_regions %>%
  dplyr::select(1:2) %>%
  rename(fips = "Fips", id = 2) %>%
  mutate(fips = ifelse(nchar(fips)==4, 
                       paste0("0",fips),
                       fips),
         state = str_sub(fips, 1, 2)) %>%
  left_join(ag_regions_key, by = "id") %>%
  left_join(state_ref_tbl, by = c('state' = 'st'))


## Generate list of land-locked states ####
no_cst_states <- noCoast()

# Loop through all FRRs ####

for(k in seq_len(nrow(ag_regions_key))) {
  
  frr_name <- ag_regions_key %>%
    filter(id == k) %>%
    pull(frr_name)
  
  cli::cli_h1(paste0("Trying: ", frr_name))
  
  cli::cli_h2("Loading Data")
  
  ## Specify states to load in ####
  states_to_load <- 
    ag_regions_ref %>%
    filter(id == k) %>%
    pull(stusps) %>%
    unique()
  
  
  ## Restrict to only counties used in nolte_county_base ####
  counties_to_include <- 
    ag_regions_ref %>%
    filter(id == k,
           fips %in% nolte_counties) %>%
    pull(fips) %>%
    sort()
  
  clean_to_load <- 
    paste0("clean_",
           states_to_load,
           ".pqt") %>%
    sort()
  
  ## Import current FRR dataframe ####
  
  cli::cli_alert_info(
    paste0(str_extract(clean_to_load, "[:upper:]{2}"), collapse = ", "))
  
  cli::cli_alert_info(
    paste0(length(counties_to_include), " counties")
  )
  
  tic('Import complete')
  
  clean_paths <- file.path(clean_dir, clean_to_load)
  
  df_import <- 
    map_dfr(clean_to_load, ~ read_parquet(.x)) %>%
    # filter to only nolte_county_base counties
    filter(fips %in% counties_to_include) %>%
    select(sid, fips, x45, y45, log_priceadj_ha, 
           dplyr::any_of(nolte2020vars)) %>%
    mutate(state = str_sub(fips, 1, 2))
  
  toc()
  
  cli::cli_alert_info(paste0("Before filtering: ", scales::comma(nrow(df_import)), 
                             " obervations"))
  
  
  imported_states <- 
    str_sub(df_import$fips, 1, 2) %>% 
    unique()
  
  imported_vars <- 
    paste(names(df_import), collapse = ", ")
  
  
  ## Replace NA cst_* with 0 for land-locked states
  
  if (str_detect(imported_vars, "cst_")) {
   
    model_df <- 
      df_import %>%
      mutate(across(.cols = starts_with("cst"),
                    .fns = ~ case_when(
                      state %in% no_cst_states & is.na(.x) ~ 0,
                      TRUE ~ .x)
                    )
             ) %>%
      dplyr::select(!c(fips, state)) %>%
      stats::na.omit()
    
  } else {
    
    model_df <- 
      df_import %>%
      mutate(cst_2500 = 0,
             cst_50 = 0) %>%
      dplyr::select(!c(fips, state)) %>%
      stats::na.omit()
      
    
  }
  
  cli::cli_alert_info(paste0("After filtering: ", scales::comma(nrow(model_df)), 
                             " obervations"))
  
  
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
  
  
  ## Collect Stats ####
  
  ### Collect Performance Stats ####
  
  cli::cli_h2("Collecting Performance Stats")
  
  tic('Performance Stats')
  
  n_obs <- nrow(model_df)
  
  n_train <- nrow(train)
  
  n_test <- nrow(test)
  
  frr_stats <- collect_metrics(rf_fit) %>%
    dplyr::select(.metric, .estimate) %>%
    spread(.metric, .estimate) %>%
    mutate(
      mse = rmse^2,
      nobs = n_obs,
      n_train = n_train,
      n_test = n_test,
      id = k) %>%
    left_join(ag_regions_key, by = "id")
  
  
  
  ### Collect Predictions ####
  
  predictions <- 
    rf_fit %>% 
    unnest(.predictions) %>% 
    select(.pred)
  
  frr_predictions <-
    bind_cols(predictions, test) %>%
    mutate(id = k,
           fips = str_sub(sid, 1, 5)) %>%
    left_join(ag_regions_key,
              by = "id") %>%
    relocate(c(id, frr_name, fips))
  
  
  ### Variable Importance ####
  
  frr_importance <- rf_fit %>%
    extract_fit_parsnip() %>%
    vip::vi() %>%
    add_row(Variable = "id",
            Importance = k) %>%
    pivot_wider(
      names_from = Variable,
      values_from = Importance
    ) %>%
    relocate(id)
  
  toc()
  
  ## Collate stats ####
  
  cli::cli_h3('Writing Stats')
  
  ## Write stats ####
  
  nfr_dir <- file.path(ddir, "model/nolte/frr/restricted")
  
  pred_dir <- file.path(nfr_dir, "predictions")
  perform_dir <- file.path(nfr_dir, "performance")
  imp_dir <- file.path(nfr_dir, "importance")
  
  pred_file <- 
    file.path(
      pred_dir, paste0("pred_nfr_", k, ".pqt")
    )
  
  perform_file <-  
    file.path(
      perform_dir, paste0("stats_nfr_", k, ".pqt")
    )
  
  imp_file <-  
    file.path(
      imp_dir, paste0("import_nfr_", k, ".pqt")
    )
  
  write_parquet(frr_predictions,
                pred_file
                )
  
  write_parquet(frr_stats,
                perform_file
                )
  
  write_parquet(frr_importance,
                imp_file
                )
  
  
  cli::cli_alert_success(paste0("Finished: ", frr_name))

}










