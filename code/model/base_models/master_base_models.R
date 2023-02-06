
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

base.mdir <- file.path(m_dir, "base_models")

# Source functions
file.path(cdir, "functions") %>%
  list.files(full.names = TRUE) %>%
  walk(source)

file.path(base.mdir, "functions") %>%
  source_dir()

# Source constants
source(file.path(base.mdir, "00_base_model_prep.R"))
