# ------------ Constants ------------------------ #
options(readr.show_col_types = FALSE)

# Nolte predictor set =========================================================
nolte2020vars <- 
  read_helper_data("nolte2020vars.csv") %>%
  select(matched_to_gold2022) %>%
  filter(!is.na(matched_to_gold2022)) %>%
  pull()

# County adjacencies ==========================================================
# -- source: https://www.nber.org/research/data/county-adjacency -- #
county_adjacency <-
  read_helper_data("county_adjacency.csv")

county_neighbors <-
  county_adjacency %>%
  split(.$fipscounty) %>%
  map("fipsneighbor")

# Nolte counties (counties modeled by Nolte's base county model) ==============
nolte_counties <-
  read_helper_data("nolte_counties.csv") %>%
  pull(fips)

# Cleaned sales data by state =================================================
all_clean <-
  list.files(
    clean_dir,
    full.names = TRUE,
    pattern = "pqt$"
  )

# State abbreviations ===========================================
all_states <-
  all_clean %>%
  str_extract("[A-Z]{2}(?=\\.pqt$)") %>%
  sort()

# State name <-> abbreviation crosswalk =======================================
state_name_abbr <- 
  read_helper_data("state_name_to_abbr.csv")

# Soil category names =========================================================
soil_vars <- 
  read_helper_data("soil_types.csv") %>%
  pull(soil_type)

# FRR id <-> name and county <-> FRR crosswalks ===============================
frr_key <- 
  read_helper_data("frr_key.csv")

county_frr_crosswalk <- 
  read_helper_data("county_frr_crosswalk.csv")

# Land-locked states ==========================================================
no_cst_states <- 
  read_helper_data("no_coast_states.csv") %>%
  pull(state_fips)

# Median home values (with imputed values in missing HPI counties) ============ 
medhomeval <- 
  read_helper_data("mhv_impute_complete.pqt")


