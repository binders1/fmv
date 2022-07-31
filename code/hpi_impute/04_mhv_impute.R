# Data prep ####

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



## Load in 2017 median home values ####
medhomeval <- read_parquet("~/fmv/data/hpi_impute/realtor/medhomeval.pqt")

medhomeval %<>%
  rename(medlistprice_2017 = "median_listing_price") %>%
  select(!year)


## Join 2017 median value with HPI and calculate median value ####
## 2000-2020 for counties that have HPI

medval_allyears_incomplete <-
  HPI_county %>%
  left_join(
    medhomeval, by = "fips"
  ) %>%
  mutate(medhomeval = HPI*medlistprice_2017) %>%
  
  select(!c(HPI, medlistprice_2017))


## Create dataframe of all counties in fmv study in all years ####

clean_path <- "~/fmv/data/cleaned"

all_clean <- file.path(clean_path,
                       list.files(clean_path))

all_study_counties <- 
  map_dfr(all_clean, read_parquet) %>%
  pull(fips) %>%
  unique()


county_year_grid <-
  expand_grid(
    fips = all_study_counties,
    year = seq(2000,2020)
    )


## Join full grid with median home value ####

all_study_with_missing <-
  county_year_grid %>%
  left_join(medval_allyears_incomplete,
            by = c("fips", "year"))

## Make vector of fips missing median home value
fips_missing_hpi <-
  all_study_with_missing %>%
  filter(is.na(medhomeval)) %>%
  pull(fips) %>%
  unique()



## Load county buffer dataframe ####
buffer_neighbors <-
  read_parquet("~/fmv/data/hpi_impute/hpi_buffer.pqt")

buffer_neighbors %<>%
  filter(fips != buffer_fips) %>%
  mutate(inv_dist_sq = 1/(dist_m^2))


# IMPUTE WITH MODEL ####

## Allocate empty tibble for imputations ####
collect_imputed_fips <- tibble()

## Allocate empty vector for num neighbors used in imputations ####
n_buf_neighbors <- vector(length = length(fips_missing_hpi))


## Loop through all missing counties and impute ####
for (i in seq_len(length(fips_missing_hpi))) {
  
  ### Grab current county to impute ####
  current_fips <- fips_missing_hpi[i]
  
  ### Grab counties within 150km buffer ####
  current_neighbors <- 
    buffer_neighbors %>%
    filter(fips == current_fips,
           buffer_fips != current_fips) %>%
    pull(buffer_fips)
  
  neighbor_distances <-
    buffer_neighbors %>%
    filter(fips == current_fips,
           buffer_fips != current_fips) %>%
    select(
      fips = "buffer_fips",
      inv_dist_sq
    )
  
  ### Take year-specific mean of medhomeval in buffer-neighbors ####
  current_imputed_fips <- 
    all_study_with_missing %>%
    filter(fips %in% current_neighbors) %>%
    left_join(neighbor_distances,
              by = "fips") %>%
    group_by(year) %>%
    summarise(mhv_imputed = weighted.mean(medhomeval,
                                          inv_dist_sq,
                                          na.rm = T)) %>%
    mutate(fips = current_fips) %>%
    relocate(fips)
  
  
  ### Collect imputed values for current county ####
  collect_imputed_fips <-
    rbind(collect_imputed_fips,
          current_imputed_fips)
  
  
  ### Collect number of buffer neighbors used ####
  n_buf_neighbors[i] <- length(current_neighbors)
  
  
  cat("\nCompleted:",current_fips," \n")
  
}



mhv_imputed <-
  all_study_with_missing %>%
  left_join(collect_imputed_fips,
            by = c("fips", "year")) %>%
  transmute(
    fips = fips,
    year = year,
    final_mhv = case_when(
      is.na(mhv_imputed) ~ medhomeval,
      TRUE ~ mhv_imputed
    ))

# Save imputed values ####

write_parquet(mhv_imputed,
              "~/fmv/data/hpi_impute/mhv_impute_complete.pqt")
