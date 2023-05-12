
# ==========================================================
# Set up
# ==========================================================

# Load packages
library(fredr)
library(tidyverse)
library(lubridate)
library(arrow)
library(sf)
`%<>%` <- magrittr::`%<>%`

# Set directory paths
root <- here::here()

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

# Source functions
file.path(cdir, "functions") %>%
  list.files(full.names = TRUE) %>%
  walk(source)

file.path(cdir, "real_estate", "functions") %>%
  source_dir()

# ==========================================================
# 01). Retrieve county-level HPI values from FRED
# ==========================================================

# Set API key as variable `FRED_API_KEY` in .Renviron file

# Get and save county HPI, 2000-2020
fred_hpi() %>%
  write_csv(
    file.path(helper_dir, "HPI_county.csv")
  )

# =========================================================
# 02). Process and save Realtor.com median home value data
# =========================================================

# This is the version used in the main models, where the 2017 MHV is used as the
# "base", from which all other years' MHV is estimated using annual county HPI 
clean_realtor(mhv_year = 2017) %>%
  write_parquet(
    file.path(mhv.dir, "medhomeval.pqt")
    )

# Below, we make version of realtor MHV data for the pred_all_parcels, which is the 
# mean of the median listing price by county. Since pred_all_parcels does not 
# have years attached to observations, we do not need to perform the HPI-assisted
# year-by-year calculation of MHV, allowing us to preserve many more counties.
# This, in turn, fixes the issue Christoph noted in our pred_all_parcels map,
# which contained ~3mil fewer observations than Nolte's version, since we had so
# many NAs, owing to the county limitations of the HPI-assisted calculation.
# See issue #59 on Github for more discussion

# 2020 values will be used for pred_all_parcels
clean_realtor(mhv_year = 2020) %>%
  write_parquet(
    file.path(helper_dir, "medhomeval_2020.pqt")
  )

# =========================================================
# 03). Compute imputation buffer zones around missing-HPI counties
# =========================================================

county_buffer(buffer_km = 150) %>%
  write_parquet(
    file.path(mhv.dir, "hpi_buffer.pqt")
  )

# =========================================================
# 04). Impute median home values and save
# =========================================================

process_mhv() %>%
  write_parquet(
    file.path(helper_dir, "mhv_impute_complete.pqt")
    )


