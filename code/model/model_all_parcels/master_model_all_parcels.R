# Specify models for parcel-level prediction ========

#' As described in clean/clean_all_parcels/master_clean_all_parcels.R,
#' this modeling pipeline builds FRR-specific models at the parcel level
#' using only parcels with no building footprint (in order to estimate the pure 
#' land value, sans built improvements). These models are then used to predict
#' the value of the all-parcel dataset of ~31 million parcels. This will then be 
#' plotted in Fig 4 (ffb_pred_all_parcels) in an effort to replicate Nolte 
#' (2020)'s Fig 1 map to show that the results of our modeling approach roughly
#' matches Nolte (2020)

# =====================================================
# Set up
# =====================================================

## Load Packages ####
library(arrow)
library(tidymodels)
library(foreach)
library(doParallel)

tidymodels_prefer()

## Set directory paths ####

root <- here::here()

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
odir <- file.path(root, "output")

f_dir <- file.path(cdir, "functions")
m_dir <- file.path(cdir, "model")

clean_dir <- file.path(ddir, "cleaned")
clean_pc_dir <- file.path(clean_dir, "cleaned_all_parcels")
helper_dir <- file.path(ddir, "helper_data")
model_ddir <- file.path(ddir, "model")

mod_pc_dir <- file.path(m_dir, "model_all_parcels")

## Load global custom functions ####

walk(
  list.files(f_dir, full.names = TRUE),
  source
  )

## Load predict_everything-related functions ####
file.path(mod_pc_dir, "functions") %>% 
  source_dir()

## Source data prep ####

file.path(mod_pc_dir, "00_model_all_parcels_prep.R") %>%
  source()


# =====================================================
#
# Model sales-level FMV paper data with no buildings
# then predict on *entire* PLACES parcel set
#
# =====================================================

# Prep parallel workers
unregisterCores()
if (foreach::getDoParWorkers() < 20) doParallel::registerDoParallel(20)

n_iters <- seq_along(frr_key$frr_name)

foreach::foreach(frr_id = n_iters) %dopar% {
  predict_all_parcels(frr_id = frr_id)
  }

unregisterCores()



