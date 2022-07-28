
## Load HPI ####
HPI_county <- 
  readr::read_csv("~/fmv/data/hpi_impute/HPIcounty.csv",
                  show_col_types = F)

HPI_county %<>% 
  mutate(state = str_sub(fips, 1, 2)) %>%
  filter(!is.element(state, c("02", "15"))) %>%
  select(!state) %>%
  mutate(across(starts_with("HPI_"), 
                ~ .x / HPI_2017)) %>%
  pivot_longer(
    cols = !fips,
    names_to = "year",
    values_to = "HPI",
    names_pattern = "^HPI_(\\d{4})"
  ) %>%
  mutate(year = as.numeric(year))


medhomeval <- read_parquet("~/fmv/data/hpi_impute/realtor/medhomeval.pqt")

medhomeval %<>%
  rename(medlistprice_2017 = "median_listing_price") %>%
  select(!year)


medval_allyears_incomplete <-
  HPI_county %>%
  left_join(
    medhomeval, by = "fips"
  ) %>%
  mutate(medhomeval = HPI*medlistprice_2017) %>%
  
  select(!c(HPI, medlistprice_2017))


