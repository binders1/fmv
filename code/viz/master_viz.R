
# =====================================================
# 01). Set up
# =====================================================

# Load packages
library(arrow)
library(RColorBrewer)
library(scales)
library(data.table)
library(ggplot2)
library(magrittr)
library(sf)
library(ggtext)
library(lubridate)

# Set directory paths
root <- here::here()

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
# 02). Generate and save exhibits (working paper)
# =====================================================

exhibit_tbl_wp <-
  dplyr::tribble(
    ~filename                    , ~.fn                    , ~device, ~width, ~height,
    "compare_county_nobs_perf"   , compare_county_nobs_perf, "png"  , 7     , 4      ,
    "FRR_map"                    , FRR_map                 , "png"  , 7     , 4      ,
    "clean_obs_density"          , clean_obs_density       , "png"  , 7     , 4      ,
    "fcb_importance_t20"         , fcb_importance_t20      , "png"  , 7     , 4      ,
    "ffb_importance_t20"         , ffb_importance_t20      , "png"  , 7     , 4      ,
    "nolte_resid_time"           , nolte_resid_time        , "png"  , 7     , 4      ,
    "county_compare_boxplot"     , county_compare_boxplot  , "png"  , 7     , 4      ,
    "compare_ffb_fcb_mse"        , compare_ffb_fcb_mse     , "png"  , 8     , 3.5    ,
    "frr_compare_mse_size"       , frr_compare_mse_size    , "png"  , 7     , 4      ,
    "ffb_pred_all_parcels"       , ffb_pred_all_parcels    , "png"  , 8     , 4      ,
    "cost_effective_30by30"      , cost_effective_30by30   , "png"  , 8     , 4      ,
    "fcb_importance_all"         , fcb_importance_all      , "png"  , 7     , 4      ,
    "ffb_importance_all"         , ffb_importance_all      , "png"  , 7     , 4      ,
    "frr_performance_size"       , frr_performance_size    , "png"  , 8     , 4      ,
  ) 

#exhibit_tbl_wp %<>% slice(11)

purrr::pwalk(
  .l = exhibit_tbl_wp,
  .f = save_image_custom
)


# =====================================================
# 02). Generate and save exhibits (PLOS One submission)
# =====================================================

# Create dataframe of all plot filenames and generating fns,
# along with image parameters (resolution (dpi), width, height, units)

exhibit_tbl_plos_one <-
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

#exhibit_tbl_plos_one %<>% slice(2)
  
purrr::pwalk(
  .l = exhibit_tbl_plos_one,
  .f = save_image_custom
)







