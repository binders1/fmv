
# =====================================================
# 01). Set up
# =====================================================

# Load packages
library(tidyverse)
library(arrow)
library(RColorBrewer)
library(scales)
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
# along with image parameters (resolution (dpi), width, height, units)

exhibit_tbl <-
  dplyr::tribble(
  ~filename, ~.fn                    , ~device, ~width, ~height, ~compression,
  "Fig1"   , compare_county_nobs_perf, "tiff" , 7     , 4      , "lzw",
  "Fig2"   , FRR_map                 , "tiff" , 7     , 4      , "lzw",
  "Fig3"   , clean_obs_density       , "tiff" , 7     , 4      , "lzw",
  "Fig4"   , ffb_pred_all_parcels    , "tiff" , 8     , 4      , "lzw",
  "Fig5"   , fcb_importance_t20      , "tiff" , 7     , 4      , "lzw",
  "Fig6"   , ffb_importance_t20      , "tiff" , 7     , 4      , "lzw",
  "Fig7"   , nolte_resid_time        , "tiff" , 7     , 4      , "lzw",
  "Fig8"   , county_compare_boxplot  , "tiff" , 7     , 4      , "lzw",
  "Fig9"   , compare_ffb_fcb_mse     , "tiff" , 8     , 3.5    , "lzw",
  "Fig10"  , frr_compare_mse_size    , "tiff" , 7     , 4      , "lzw",
  "Fig11"  , cost_effective_30by30   , "tiff" , 8     , 4      , "lzw",
  "S1"     , fcb_importance_all      , "tiff" , 7     , 4      , "lzw",
  "S2"     , ffb_importance_all      , "tiff" , 7     , 4      , "lzw",
  "S3"     , frr_performance_size    , "tiff" , 8     , 4      , "lzw"
  ) 

#exhibit_tbl %<>% slice(2)
  
purrr::pwalk(
  .l = exhibit_tbl,
  .f = save_image_custom
)







