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
  source)

#=======================================================
# 02). Load auxiliary datasets and helper vectors/lists 
#=======================================================

file.path(pc_cleaning.dir, "00_clean_prep.R") %>% source()

#=======================================================
# 03). Process each state's data 
#=======================================================

# Source cleaning functions 
walk(
  .x = list.files(cleaning_fdir, full.names = TRUE),
  .f = source
)

# Map processing function over all state files

state_seq <- 
  pqt_dir %>% 
  list.files() %>% 
  seq_along()

walk(
  .x = state_seq,
  .f = process_state.pc
)
