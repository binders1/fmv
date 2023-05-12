# =====================================================
# 00). Set up
# =====================================================

# Load libraries 
library(arrow)
library(base)
library(butcher)
library(cli)
library(data.table)
library(doParallel)
library(dplyr)
library(foreach)
library(fredr)
library(fs)
library(ggplot2)
library(ggtext)
library(grDevices)
library(grid)
library(here)
library(janitor)
library(lubridate)
library(magrittr)
library(parsnip)
library(patchwork)
library(purrr)
library(ranger)
library(RColorBrewer)
library(readr)
library(recipes)
library(renv)
library(rsample)
library(rstudioapi)
library(scales)
library(sf)
library(stringr)
library(tictoc)
library(tidymodels)
library(tidyr)
library(tools)
library(utils)
library(vip)

# Set directory paths
root <- here::here()
cdir <- file.path(root, "code")

real_estate_dir <- file.path(cdir, "real_estate")

cleaning.dir <- file.path(cdir, "clean")
pc_cleaning.dir <- file.path(cleaning.dir, "clean_all_parcels")

m_dir <- file.path(cdir, "model")
base.mdir <- file.path(m_dir, "base_models")
mod_pc_dir <- file.path(m_dir, "model_all_parcels")

v.dir <- file.path(cdir, "viz")

# Source general functions used throughout project 
file.path(cdir, "functions") %>%
  list.files(full.names = TRUE) %>%
  walk(source)

# ==============================================================================
# 01). Download and process real estate indicators (HPI and Median Home Value)
# ==============================================================================
real_estate_dir %>%
  file.path("master_real_estate.R") %>%
  source()

# ==============================================================================
# 02). Clean sale-level data for main analysis
# ==============================================================================
cleaning.dir %>%
  file.path("master_clean.R") %>%
  source()

# ==============================================================================
# 03). Clean all parcel-level data for Nolte CONUS sale value map replication
# ==============================================================================
pc_cleaning.dir %>%
  file.path("master_clean_all_parcels.R") %>%
  source()

# ==============================================================================
# 04). Run main suite of sale-level models at county and FRR levels
# ==============================================================================
base.mdir %>%
  file.path("master_base_models.R") %>%
  source()

# ==============================================================================
# 05). Generate predicted values for all parcels, using an FRR model w/o bldgs
# ==============================================================================
mod_pc_dir %>%
  file.path("master_model_all_parcels.R") %>%
  source()

# ==============================================================================
# 06). Generate paper exhibits
# ==============================================================================
v.dir %>%
  file.path("master_viz.R") %>%
  source()

