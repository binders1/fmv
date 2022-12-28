## Nolte File Names ####

nolte_files <- data.frame(name = list.files(nolte.dir))

### subset to only parcel data ####
all_pc <- 
  nolte_files %>%
  filter(str_detect(name, "_pc\\.pqt$")) %>%
  pull(name) %>%
  sort()

## Retrieve New Soil farmlncl <-> new-category crosswalk ####
soil_crosswalk <- read_helper_data("soil_crosswalk.csv")

## Retrieve vars used in Nolte (2020) ####
nolte2020vars_df <- 
  read_helper_data("nolte2020vars.csv")

nolte2020vars <-
  nolte2020vars_df %>%
  dplyr::filter(!stringr::str_detect(matched_to_gold2022, "\\+")) %>%
  pull(matched_to_gold2022)

### Specify variables for aggregation ####

# Climate variables to (weighted) average
climate_to_mean <- 
  read_helper_data("climatevars_to_mean.csv") %>%
  pull(variable_name)

# Load HPI Index ####
HPI_county_pc <- 
  read_helper_data("HPI_county.csv") %>%
  mutate(across(HPI_2000:HPI_2020, ~ .x/HPI_2020)) %>%
  pivot_longer(
    cols = HPI_2000:HPI_2020,
    names_to = "year",
    values_to = "HPI"
  ) %>%
  group_by(fips) %>%
  summarise(HPI = mean(HPI, na.rm = TRUE))

# Load imputed median home values by fips-year
mhv <- read_helper_data("mhv_impute_complete.pqt")

mhv_mean <-
  mhv %>%
  group_by(fips) %>%
  summarise(MedianHomeValue = mean(MedianHomeValue, na.rm = TRUE)) %>%
  ungroup()
