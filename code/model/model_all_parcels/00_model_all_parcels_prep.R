
## Load soil category names ####
soil_vars <- 
  read_helper_data("soil_types.csv") %>%
  pull(soil_type)

## State abbr-to-number ref table ####
state_ref_tbl <- read_helper_data("state_name_to_abbr.csv")

## Build FRR-county reference table ####
frr_key <- read_helper_data("frr_key.csv")
  
county_frr_crosswalk <- read_helper_data("county_frr_crosswalk.csv")

## Read in list of land-locked states ####
no_cst_states <- read_helper_data("no_coast_states.csv") %>%
  pull(state_fips)

## Load (imputed) median home value #### 
medhomeval <- read_helper_data("mhv_impute_complete.pqt")
