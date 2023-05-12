
# Main suite of 7 models reported in paper ============
#' The models vary by predictor set (full vs. nolte), modeling geography (county
#' vs. FRR), and additional geographic-specific adjustments. 
#'  - At the county level, one Nolte predictor set model includes HPI. 
#'  - At the FRR level, both predictor set models are run with a sample that 
#'    includes only observations in counties that were modeled by the Nolte 
#'    county model (i.e., they met the 1000-observation minimum, either on 
#'    their own or with neighbor donations)
#'    
#' For more, read https://github.com/AMGold99/fmv/issues/51 and its 
#' associated PR: https://github.com/AMGold99/fmv/pull/52. 

# Set up ======================================================================

# Load Packages
library(arrow)
library(tidymodels)
library(ranger)
library(foreach)
library(tictoc)

tidymodels_prefer()

# Set directory paths

root <- here::here()

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



# County models ===============================================================

tic("fcb") # Full County Base
fmv_model(geo = "county", pred.set = "full", HPI = TRUE)
toc()

tic("ncb") # Nolte County Base
fmv_model(geo = "county", pred.set = "nolte", HPI = FALSE)
toc()

tic("nch") # Nolte County HPI
fmv_model(geo = "county", pred.set = "nolte", HPI = TRUE)
toc()


# FRR models ==================================================================

tic("ffb") # Full FRR Base
fmv_model(geo = "frr", pred.set = "full", only.nolte.counties = FALSE)
toc()

tic("ffr") # Full FRR Restricted
fmv_model(geo = "frr", pred.set = "full", only.nolte.counties = TRUE)
toc()

tic("nfb") # Nolte FRR Base
fmv_model(geo = "frr", pred.set = "nolte", only.nolte.counties = FALSE)
toc()

tic("nfr") # Nolte FRR Restricted
fmv_model(geo = "frr", pred.set = "nolte", only.nolte.counties = TRUE)
toc()
