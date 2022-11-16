
# ==========================================================
# Set up
# ==========================================================

# Load packages
library(fredr)
library(tidyverse)
library(lubridate)
library(arrow)
library(sf)

# Set directory paths
root <- "~/fmv"

## first level
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

## second level
f.dir <- file.path(cdir, "functions")
mhv.dir <- file.path(ddir, "mhv_impute")
s.dir <- file.path(ddir, "spatial")
clean.dir <- file.path(ddir, "cleaned")
helper_dir <- file.path(ddir, "helper_data")

## third level
state_dir <- file.path(s.dir, "state_shp")
county_dir <- file.path(s.dir, "county_shp")



# ==========================================================
# 01). Retrieve county-level HPI values from FRED
# ==========================================================

# Set API key ####
Sys.setenv(FRED_API_KEY = "db828b951775e7f2dc8cc3c88541a117")

# Get and save county HPI, 2000-2020
fred_hpi() %>%
  write_csv(
    file.path(helper_dir, "HPI_county.csv")
  )

# =========================================================
# 02). Process and save Realtor.com median home value data
# =========================================================

clean_realtor() %>%
  write_parquet(
    file.path(mhv_dir, "medhomeval.pqt")
    )

# =========================================================
# 03). Compute imputation buffer zones around missing-HPI counties
# =========================================================

county_buffer(buffer_km = 150) %>%
  write_parquet(
    file.path(mhv_dir, "hpi_buffer.pqt")
  )

# =========================================================
# 04). Impute median home values and save
# =========================================================

process_mhv() %>%
  write_parquet(
    file.path(helper_dir, "mhv_impute_complete.pqt")
    )


