## Nolte File Names ####

nolte_files <- data.frame(name = list.files(nolte.dir))

### subset to only sales data ####
all_sale <- 
  nolte_files %>%
  filter(str_detect(name, "_sale\\.pqt$")) %>%
  pull(name)

### subset to only sale_pids crosswalk files ####
all_sale_pids <- 
  nolte_files %>%
  filter(str_detect(name, "sale_pids")) %>%
  pull(name)

## Retrieve New Soil farmlncl <-> new-category crosswalk ####
# TODO: make local file
soil_crosswalk <- 
  googlesheets4::range_read(
    ss = "1AJlJgiMgMQXB9kNKMRuVP6_f5D60-bPmnBMGVYpVPYs",
    sheet = "New Categories"
  )

## Retrieve vars used in Nolte (2020) ####
nolte2020vars_df <- 
  read_csv(file.path(ddir, "nolte2020vars.csv"),
           show_col_types = FALSE)

nolte2020vars <- 
  nolte2020vars_df %>%
  dplyr::filter(!stringr::str_detect(nolte_2020, "\\+")) %>%
  dplyr::pull(nolte_2020)

### Specify variables for aggregation ####

extract_for_agg <- function(agg_method) {
  
  nolte2020vars_df %>%
    dplyr::filter(aggregate_method == agg_method) %>%
    dplyr::pull(matched_to_gold2022)
  
}

# Nolte variables to average 
noltevars_to_mean <- extract_for_agg("Mean")

# Nolte variables to sum
noltevars_to_sum <- extract_for_agg("Sum")

# Climate variables to (weighted) average
climate_example_loc <- 
  list.files(pqt_dir, 
             pattern = "AL\\.pqt$", 
             full.names = TRUE)

climate_to_mean <- 
  read_parquet(climate_example_loc) %>%
  dplyr::select(
    dplyr::matches('(^Dew|^Temp|^Precip)')
  ) %>%
  names()


## Load CPI data ####
CPI <- 
  read_csv(file.path(ddir, 'CPIAUCSL.csv'),
           show_col_types = FALSE) %>%
  rename(CPI = "CPIAUCSL") %>%
  mutate(year = lubridate::year(DATE),
                month = lubridate::month(DATE)) %>%
  dplyr::filter(year >= 2000 & year <= 2020) %>%
  
  mutate(
    CPI = CPI/CPI[year == 2020 & month == 1]
  )

# Load HPI Index ####
HPI_county <- 
  readr::read_csv(file.path(ddir, "mhv_impute", 'HPIcounty.csv'),
                  show_col_types = F) %>%
  mutate(across(HPI_2000:HPI_2020, ~ .x/HPI_2020)) %>%
  pivot_longer(
    cols = HPI_2000:HPI_2020,
    names_to = "year",
    values_to = "HPI"
  ) %>%
  mutate(year = as.numeric(str_remove(year, "HPI_")))

