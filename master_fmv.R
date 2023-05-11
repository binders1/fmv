# =====================================================
# 00). Set up
# =====================================================

# Load libraries 
library(magrittr)
library(fs)

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
  list.files(full.names = TRUE)

# Create empty data directory, which should then be filled with 
# the data_dirs below (as saved in some data repository)
fs::dir_create("~/fmv/data")

#' @TODO: Discuss data repository/data sharing of the data contained in Nolte/

data_dirs <-
    c("Nolte", "ArcResults", "mhv_impute",
      "spatial", "helper_data")

if (!all(data_dirs %in% list.files(ddir))) {
  stop(
    "Cannot continue without the following data in ~/fmv/data:\n ",
    paste0(data_dirs, sep = "/", collapse = " \n "))
  }

readr::read_lines(
  file.path(root, "data", "helper_data", "model_dirs.txt")
) %>%
  fs::dir_create()

fs::dir_create(
  file.path(
    root, "data", "cleaned"
  )
)

fs::dir_create(
  file.path(
    root, "data", "cleaned", "cleaned_all_parcels"
  )
)

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

