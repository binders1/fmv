
# 

process_mhv <- function() {
  
  # ======================================================
  # Load and clean HPI
  # ======================================================
  
  HPI_county <- read_helper_data("HPI_county.csv")
  
  # Create HPI panel by county and year
  HPI_county %<>% 
    # Create state fips indicator
    mutate(state = str_sub(fips, 1, 2)) %>%
    # Remove Alaska and Hawaii
    filter(!is.element(state, c("02", "15"))) %>%
    select(!state) %>%
    
    # Normalize on 2017 values
    mutate(across(starts_with("HPI_"), 
                  ~ .x / HPI_2017)) %>%
    
    # Pivot so that years become rows
    pivot_longer(
      cols = !fips,
      names_to = "year",
      values_to = "HPI",
      names_pattern = "^HPI_(\\d{4})"
    ) %>%
    mutate(year = as.numeric(year))
  
  # ======================================================
  # Load and clean 2017 median home values ####
  # ======================================================
  medhomeval <-
    read_parquet(
      file.path(mhv.dir, "medhomeval.pqt")
    )
  
  medhomeval %<>%
    rename(medlistprice_2017 = "median_listing_price") %>%
    select(!year)
  
  
  # Join 2017 median value with all-years HPI and calculate 
  # median value 2000-2020 for counties that have HPI
  
  medval_allyears_incomplete <-
    HPI_county %>%
    left_join(
      medhomeval, by = "fips"
    ) %>%
    mutate(medhomeval = HPI*medlistprice_2017) %>%
    
    select(!c(HPI, medlistprice_2017))
  
  # ==================================================================
  # Panel of all counties in fmv study in all years
  # 
  # This allows us to tell which counties are missing HPI (and thus 
  # MHV) Those counties will get their MHV imputed based on a inverse 
  # square distance-weighted average among all counties whose 
  # centroids are within 150km of the county with the missing MHV 
  # value
  # ==================================================================
  
  all_clean <- 
    list.files(clean.dir,
               full.names = TRUE,
               pattern = "pqt$")
  
  all_study_counties <- 
    map_dfr(all_clean, read_parquet) %>%
    pull(fips) %>%
    unique()
  
  county_year_panel <-
    expand_grid(
      fips = all_study_counties,
      year = seq(2000,2020)
    )
  
  ## Join full county-year panel with median home value ####
  
  all_study_with_missing <-
    county_year_panel %>%
    left_join(medval_allyears_incomplete,
              by = c("fips", "year"))
  
  ## Make vector of fips missing median home value
  fips_missing_hpi <-
    all_study_with_missing %>%
    filter(is.na(medhomeval)) %>%
    pull(fips) %>%
    unique()
  
  # ======================================================
  # Inverse square distance from focal county
  # ======================================================
  
  # Load buffers
  buffer_neighbors <-
    read_parquet(
      file.path(mhv.dir, "hpi_buffer.pqt")
    )
  
  ## Calculate Inverse Squared Distance from focal centroid
  buffer_neighbors %<>%
    filter(fips != neighbor_fips) %>%
    mutate(inv_dist_sq = 1/(dist_m^2))
  
  # ======================================================
  # Impute MHV using weighted mean from buffer counties
  # ======================================================
  
  # Map imputation over all HPI-missing counties
  # See functions/imputation_helpers.R for the definition of impute_pipeline()
  collect_imputed <-
    fips_missing_hpi %>%
    map_dfr(
      ~ impute_pipeline(focal_fips = .x, 
                        buffer_neighbors = buffer_neighbors,
                        all_study_with_missing = all_study_with_missing)
    )
  
  # Merge with all study counties and fill in missing values
  # and return dataframe
  all_study_with_missing %>%
    left_join(collect_imputed,
              by = c("fips", "year")) %>%
    transmute(
      fips = fips,
      year = year,
      MedianHomeValue = case_when(
        is.na(mhv_imputed) ~ medhomeval,
        TRUE ~ mhv_imputed
      ))
  
}