
# =====================================================
# 01). Set up
# =====================================================

# Load packages
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
clean.dir <- file.path(ddir, "cleaned")
s.dir <- file.path(ddir, "spatial")
  
  state_dir <- file.path(s.dir, "state_shp")
  county_dir <- file.path(s.dir, "county_shp")

  exhibitfn_dir <- file.path(v.dir, "exhibit_fns") 

# Source custom functions ####
fns_to_source <- 
  list.files(f.dir, full.names = TRUE)

purrr::walk(fns_to_source, source)

# Source exhibit (FRR, spatial, etc.)
file.path(v.dir, "00_exhibit_prep.R") %>% source()


# =====================================================
# 02). Generate and save exhibits
# =====================================================


# source all plot fns 
plot_fns_source <- 
  list.files(exhibitfn_dir, 
             full.names = TRUE)

purrr::walk(plot_fns_source, source)


# Create dataframe of all plot filenames and generating fns

exhibit_tbl <-
  dplyr::tribble(
    ~filename                 , ~.fn,
    "compare_county_nobs_perf", compare_county_nobs_perf,
    "FRR_map"                 , FRR_map,
    "clean_obs_density"       , clean_obs_density, # TODO: test
    "fcb_importance_t20"      , fcb_importance_t20,
    "ffb_importance_t20"      , ffb_importance_t20,
    "nolte_resid_time"        , nolte_resid_time,
    "county_compare_boxplot"  , county_compare_boxplot,
    "compare_ffb_fcb_mse"     , compare_ffb_fcb_mse,
    "frr_compare_mse_size"    , frr_compare_mse_size,
    "fcb_importance_all"      , fcb_importance_all,
    "ffb_importance_all"      , ffb_importance_all,
    "frr_performance_size"    , frr_performance_size
  )

exhibit_tbl <- 
  exhibit_tbl %>%
  slice(5)
  
purrr::pwalk(
  .l = exhibit_tbl,
  .f = save_png_custom
)



