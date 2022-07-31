library(tidyverse)

# Source custom function(s) ####

source("~/fmv/code/functions/sourceFuncs.R")
sourceFuncs()


# Specify plot parameters ####


## Plot color palettes ####
rsqcolors <- c('#FBFDD0', '#40B5C4','#081D59')

msecolors <- c('#549A79', '#FDF2A9', '#C3546E')


## Load plot fonts ####

loadFont("Source Sans Pro", "IBM Plex Sans", "Lato")

## Specify state and FRR vectors ####

states <- list.files("~/fmv/data/cleaned") %>%
  str_extract("[:upper:]{2}") %>%
  sort()

frr_vec <- as.character(seq_len(9))





# Load model dataframes ####


## County models ####

### Performance ####

setwd("~/fmv/data/model/county")


#### Nolte performance ####
county_perform_nolte <- modImport(parent_dir = "nolte", 
                                  stat_dir = "performance",
                                  file_prefix = "stats_", 
                                  file_suffix = states,
                                  add_source = "Nolte")

#### Full performance ####
county_perform_full <- modImport(parent_dir = "rf", 
                                 stat_dir = "performance",
                                 file_prefix = "stats_", 
                                 file_suffix = states,
                                 add_source = "Full")



### Predictions #### 

#### Nolte Predictions ####

county_pred_nolte <- modImport(parent_dir = "nolte", 
                               stat_dir = "predictions",
                               file_prefix = "pred_", 
                               file_suffix = states,
                               add_source = "Nolte")

#### Full Predictions ####

county_pred_full <- modImport(parent_dir = "rf", 
                              stat_dir = "predictions",
                              file_prefix = "pred_", 
                              file_suffix = states,
                              add_source = "Full")


### Importance ####


#### Nolte Importance ####

county_imp_nolte <- modImport(parent_dir = "nolte", 
                              stat_dir = "importance",
                              file_prefix = "import_", 
                              file_suffix = states,
                              add_source = "Nolte")

#### Full Importance ####

county_imp_full <- modImport(parent_dir = "rf", 
                             stat_dir = "importance",
                             file_prefix = "import_", 
                             file_suffix = states,
                             add_source = "Full")




## FRR Models ####

setwd("~/fmv/data/model/FRR")


### Performance ####

#### Nolte Performance ####

frr_perform_nolte

#### Full Performance ####

frr_perform_full <- modImport(parent_dir = "rf", 
                              stat_dir = "performance",
                              file_prefix = "stats_frr_", 
                              file_suffix = frr_vec,
                              add_source = "Full")


### Predictions ####

#### Nolte Predictions ####

frr_pred_nolte

#### Full Predictions ####

frr_pred_full <- modImport(parent_dir = "rf", 
                           stat_dir = "predictions",
                           file_prefix = "pred_frr_", 
                           file_suffix = frr_vec,
                           add_source = "Full")


### Importance ####

#### Nolte Importance ####

frr_imp_nolte

#### Full Importance ####

frr_imp_full <- modImport(parent_dir = "rf", 
                          stat_dir = "importance",
                          file_prefix = "import_frr_", 
                          file_suffix = frr_vec,
                          add_source = "Full")


