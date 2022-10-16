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

## second level
nolte.dir <- file.path(ddir, "Nolte")
m.dir <- file.path(ddir, "model")
clean.dir <- file.path(ddir, "cleaned")
arc.dir <- file.path(ddir, "ArcResults")
f.dir <- file.path(cdir, "functions")
cleaning.dir <- file.path(cdir, "clean")

## third level
pqt_dir <- file.path(arc.dir, "parquet")
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
walk(
  list.files(cleaning_fdir, full.names = TRUE),
  source
)

# Map processing function over all state files

pcis_pqt <- list.files(pqt_dir)

state_seq <- seq_along(pcis_pqt)

walk(
  .x = state_seq,
  .f = process_state
)


