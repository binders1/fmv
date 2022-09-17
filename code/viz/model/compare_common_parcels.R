# Load package ####
library(tidyverse)
library(sf)
# Set up ####
root <- "~/fmv"
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

# Source Prep ####
source(file.path(cdir, "functions/sourceFuncs.R"))
sourceFuncs()
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(cdir, "viz/model/frr/viz_frr_prep.R"))


# Load font ####
font <- "Open Sans"
loadFont(font)

# Function that compares MSE across common parcels of two models

comparePerf <- function(mod1, mod2) {
  
  models <- list(mod1, mod2)
  
  pred_data <- 
    purrr::map_dfr(
      models,
      loadResults,
      res_type = "predictions"
    )
  
  uniqueSid <- function(.model, data) {
    
    data %>%
      dplyr::filter(model == .model) %>%
      pull(sid) %>%
      unique()
    
  }
  
  unique_sids <-
    map(models, 
        uniqueSid, pred_data)
  
  common_parcels <-
    base::intersect(unique_sids[[1]],
                    unique_sids[[2]])
  
  pred_data %>%
    filter(sid %in% common_parcels) %>%
    mutate(sq_error = ( .pred - log_priceadj_ha )^2) %>%
    group_by(model) %>%
    summarise(mse = mean(sq_error)) %>%
    pivot_wider(
      names_from = model,
      values_from = mse
    ) %>%
    mutate(improvement = ( .data[[mod2]] - .data[[mod1]] )/.data[[mod1]],
           improvement = scales::label_percent()(improvement))
  
  
}

# Compare ncb --> ffb (added variables and FRR modeling)
ncb_to_ffb <- comparePerf("ncb", "ffb")

# Compare ncb --> fcb (added variables)
ncb_to_fcb <- comparePerf("ncb", "fcb")

# Compare fcb --> ffb (FRR modeling)
fcb_to_ffb <- comparePerf("fcb", "ffb")




