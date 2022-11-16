
## Load soil category names ####
soil_vars <- read_helper_data("soil_types.csv",
                              show_col_types = FALSE) %>%
  pull(soil_type)

## State abbr-to-number ref table ####
state_ref_tbl <- read_helper_data("state_ref_tbl.csv",
                                  show_col_types = FALSE)

## Build FRR-county reference table ####
frr_key <- read_helper_data("frr_key.csv", 
                            show_col_types = FALSE)
  
county_frr_crosswalk <- read_helper_data("county_frr_crosswalk.csv", 
                                         show_col_types = FALSE)

## Read in list of land-locked states ####
no_cst_states <- read_helper_data("no_coast_states.csv",
                                  show_col_types = FALSE) %>%
  pull(state_fips)


## Load (imputed) median home value #### 
medhomeval <- read_helper_data("mhv_impute_complete.pqt")
