# Clean parcel-level PLACES data. ====
# 
#' Unlike the basic cleaning performed in clean/master_clean.R,
#' this pipeline does not join parcels with sales, and thus does not aggregate
#' variables to the sale level. These parcel observations (~31million) are used
#' to generate Fig 4 (ffb_pred_all_parcels), where estimated costs are predicted
#' using a building-free FRR full-predictor set model specified in 
#' model/model_all_parcels
#' 
#' For more, read https://github.com/AMGold99/fmv/issues/42 and its associated
#' PR: https://github.com/AMGold99/fmv/pull/46. The issue/PR/commit messages
#' explain the approach to the broader project of "replicating" Nolte (2020)'s 
#' Fig 1 using the data we possess


# =====================================================
# 01). Set up
# =====================================================

# Load packages
library(lubridate)
library(magrittr)
library(arrow)
library(data.table)
library(sf)
library(tictoc)

# Set directory paths
root <- here::here()

## first level
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

## data related
nolte.dir <- file.path(ddir, "Nolte")
m.dir <- file.path(ddir, "model")
pc_clean.dir <- file.path(ddir, "cleaned", "cleaned_all_parcels")
helper_dir <- file.path(ddir, "helper_data")

arc.dir <- file.path(ddir, "ArcResults")
pqt_dir <- file.path(arc.dir, "parquet")
soil_dir <- file.path(arc.dir, "soilcodes")

## code related
f.dir <- file.path(cdir, "functions")
pc_cleaning.dir <- file.path(cdir, "clean", "clean_all_parcels")
cleaning_fdir <- file.path(pc_cleaning.dir, "functions")


# Source custom global functions 
walk(
  list.files(f.dir, full.names = TRUE),
  source
  )

#=======================================================
# 02). Load auxiliary datasets and helper vectors/lists 
#=======================================================

file.path(pc_cleaning.dir, "00_clean_all_parcels_prep.R") %>% 
  source()

#=======================================================
# 03). Process each state's data 
#=======================================================

# Source cleaning functions 
source_dir(cleaning_fdir)

# Map processing function over all state files
state_seq <- seq_along(pcis_pqt)

walk(
  .x = state_seq,
  .f = process_state.pc
)
