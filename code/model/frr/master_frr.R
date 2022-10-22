
# =====================================================
# Set up
# =====================================================

## Load Packages ####
library(tidyverse)
library(arrow)
library(tidymodels)

tidymodels_prefer()

## Set directory paths ####

root <- "~/fmv"

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
odir <- file.path(root, "output")

f_dir <- file.path(cdir, "functions")
m_dir <- file.path(cdir, "model")

clean_dir <- file.path(ddir, "cleaned")
helper_dir <- file.path(ddir, "helper_data")

frr_mod_dir <- file.path(cdir, "model", "frr")

## Load custom functions ####

walk(
  list.files(f_dir, full.names = TRUE),
  source
  )

## Source data prep ####
file.path(frr_mod_dir, "00_frrmod_prep.R") %>% source()

# =====================================================
# 01). Import FRR data, select vars, filter counties
# =====================================================
frr_import(3)

# =====================================================
# 02). Run FRR extremely randomized tree model
# =====================================================


# =====================================================
# 03). Save results to disk
# =====================================================













