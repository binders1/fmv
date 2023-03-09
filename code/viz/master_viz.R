
# =====================================================
# 01). Set up
# =====================================================

# Load packages
library(geofacet)
library(tidyverse)
library(arrow)
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(magrittr)
library(sf)
library(ggtext)
library(googlesheets4)
library(lubridate)

# Set directory paths
root <- "~/fmv"

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
odir <- file.path(root, "output")

v.dir <- file.path(cdir, "viz")
f.dir <- file.path(cdir, "functions")

e.dir <- file.path(odir, "exhibits")

m.dir <- file.path(ddir, "model")
clean_dir <- clean.dir <- file.path(ddir, "cleaned")

s.dir <- file.path(ddir, "spatial")
helper_dir <- file.path(ddir, "helper_data")
  
  state_dir <- file.path(s.dir, "state_shp")
  county_dir <- file.path(s.dir, "county_shp")

  exhibitfn_dir <- file.path(v.dir, "exhibit_fns") 

  
  
# Source custom functions ####
  
## General custom functions
list.files(f.dir, full.names = TRUE) %>%
    purrr::walk(source)

## Plot-specific functions 
list.files(exhibitfn_dir, full.names = TRUE) %>%
  purrr::walk(source)


# Source exhibit (FRR, spatial, etc.)
file.path(v.dir, "00_exhibit_prep.R") %>% source()


# =====================================================
# 02). Generate and save exhibits
# =====================================================

# Create dataframe of all plot filenames and generating fns,
# along with png parameters (resolution (dpi), width, height, units)

exhibit_tbl <-
  dplyr::tribble(
  ~filename                 , ~.fn                    , ~width, ~height,
  
  "compare_county_nobs_perf"   , compare_county_nobs_perf, 7     , 4  ,
  "FRR_map"                    , FRR_map                 , 7     , 4  ,
  "clean_obs_density"          , clean_obs_density       , 7     , 4  ,
  "fcb_importance_t20"         , fcb_importance_t20      , 7     , 4  ,
  "ffb_importance_t20"         , ffb_importance_t20      , 7     , 4  ,
  "nolte_resid_time"           , nolte_resid_time        , 7     , 4  ,
  "county_compare_boxplot"     , county_compare_boxplot  , 7     , 4  ,
  "compare_ffb_fcb_mse"        , compare_ffb_fcb_mse     , 8     , 3.5,
  "frr_compare_mse_size"       , frr_compare_mse_size    , 7     , 4  ,
  "ffb_pred_all"               , ffb_pred_all            , 8     , 4  ,
  "ffb_pred_all_nobldg"        , ffb_pred_all_nobldg     , 8     , 4  ,
  "ffb_pred_all_parcels"       , ffb_pred_all_parcels    , 8     , 4  ,
  "fcb_importance_all"         , fcb_importance_all      , 7     , 4  ,
  "ffb_importance_all"         , ffb_importance_all      , 7     , 4  ,
  "frr_performance_size"       , frr_performance_size    , 8     , 4  ,
  "cost_effective_30by30"      , cost_effective_30by30   , 7     , 4  ,
  "average_error_by_model"     , average_error_by_model  , 9     , 5  ,
  "model_error_by_decile"      , model_error_by_decile   , 7     , 4
  ) 

exhibit_tbl %<>% slice(12)
  
purrr::pwalk(
  .l = exhibit_tbl,
  .f = save_png_custom
)







