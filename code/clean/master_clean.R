# =====================================================
# 01). Set up
# =====================================================

# Load packages
library(arrow)
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(magrittr)
library(sf)
library(ggtext)
library(lubridate)

# Set directory paths
root <- here::here()

## first level
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

## second level
nolte.dir <- file.path(ddir, "Nolte")
m.dir <- file.path(ddir, "model")
clean.dir <- file.path(ddir, "cleaned")
arc.dir <- file.path(ddir, "ArcResults")
helper_dir <- file.path(ddir, "helper_data")

f.dir <- file.path(cdir, "functions")
cleaning.dir <- file.path(cdir, "clean")

## third level
pqt_dir <- file.path(arc.dir, "parquet")
soil_dir <- file.path(arc.dir, "soilcodes")
cleaning_fdir <- file.path(cleaning.dir, "functions")


# Source custom global functions 
walk(
  list.files(f.dir, full.names = TRUE),
  source)

#=======================================================
# 02). Load auxiliary datasets and helper vectors/lists 
#=======================================================

file.path(cleaning.dir, "00_clean_prep.R") %>% source()

#=======================================================
# 03). Process each state's data 
#=======================================================

# Source cleaning functions 
source_dir(cleaning_fdir)

# Map processing function (process_state()) over all state datasets

pcis_pqt <- list.files(pqt_dir)

state_seq <- seq_along(pcis_pqt)

walk(
  .x = state_seq,
  .f = process_state
)


