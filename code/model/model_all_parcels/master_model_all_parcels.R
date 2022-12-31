
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
clean_pc_dir <- file.path(clean_dir, "cleaned_all_parcels")
helper_dir <- file.path(ddir, "helper_data")
model_ddir <- file.path(ddir, "model")

mod_pc_dir <- file.path(m_dir, "model_all_parcels")

## Load global custom functions ####

walk(
  list.files(f_dir, full.names = TRUE),
  source
  )

## Load predict_everything-related functions ####
file.path(mod_pc_dir, "functions") %>%
  list.files(full.names = TRUE) %>%
  walk(source)

## Source data prep ####

file.path(mod_pc_dir, "00_model_all_parcels_prep.R") %>%
  source()


# =====================================================
#
# Run ERT models, with and without buildings,
# then predict on entire (bldg or no) FRR data and save
#
# =====================================================

walk(
  .x = seq(9),
  predict_all_parcels
  ) 



