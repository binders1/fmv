# Specify plot parameters ####


## Plot color palettes ####
rsqcolors <- c('#FBFDD0', '#40B5C4','#081D59')

msecolors <- c('#549A79', '#FDF2A9', '#C3546E')


## Load plot fonts ####

loadFont("Source Sans Pro", "IBM Plex Sans")


# Load dataframes ####

## County models ####

### Performance ####

setwd("~/fmv/data/model/county")


#### Nolte performance ####
county_perform_nolte <- map_dfr(
  states,
  ~ read_parquet(paste0("nolte/performance/stats_", .x, ".pqt"))) %>%
  mutate(across(n_train:n_test, .fns = log, .names = "log_{col}")) %>%
  mutate(source = "Nolte")

#### Full performance ####
county_perform_full <- map_dfr(
  states,
  ~ read_parquet(paste0("rf/performance/stats_", .x, ".pqt"))) %>%
  mutate(across(n_train:n_test, .fns = log, .names = "log_{col}")) %>%
  mutate(source = "Full")



### Predictions #### 

#### Nolte Predictions ####

county_pred_nolte

#### Full Predictions ####

county_pred_full


### Importance ####


#### Nolte Importance ####

county_imp_nolte

#### Full Importance ####

county_imp_full




## FRR Models ####

setwd("~/fmv/data/model/FRR")


### Performance ####

#### Nolte Performance ####

frr_perform_nolte

#### Full Performance ####

frr_perform_full


### Predictions ####

#### Nolte Predictions ####

frr_pred_nolte

#### Full Predictions ####

frr_pred_full


### Importance ####

#### Nolte Importance ####

frr_imp_nolte

#### Full Importance ####

frr_imp_full


