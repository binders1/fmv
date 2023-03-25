
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
model_ddir <- file.path(ddir, "model")

pred_evry_dir <- file.path(m_dir, "predict_everything")

## Load global custom functions ####

walk(
  list.files(f_dir, full.names = TRUE),
  source
  )

## Load predict_everything-related functions ####
walk(
  list.files(file.path(pred_evry_dir, "functions"), 
             full.names = TRUE),
  source
)

## Source data prep ####
file.path(pred_evry_dir, 
          "00_predict_everything_prep.R") %>% 
  source()


# =====================================================
#
# Run ERT models, with and without buildings,
# then predict on entire (bldg or no) FRR data and save
#
# =====================================================

walk2(
  .x = rep(seq(9), 2) %>% sort(),
  .y = rep(c(TRUE, FALSE), 9),
  import_and_model
  ) 



