# Median House Prices ============================####


# Set API key ####
Sys.setenv(CENSUS_KEY="24a0e6e31ba71d8b3e0f70ba0b4037fd194d6aec")

medHomeVal <- function() {
  
  # Retrieve median home value census data ####
  homevalue <- 
    censusapi::getCensus(name = "acs/acs5",
                         vintage = 2020, 
                         vars = "B25077_001E", 
                         region = "county:*") %>% 
    dplyr::filter(!is.element(state, c('02','15','72'))) %>%
    dplyr::mutate(fips = paste0(state,county)) %>%
    dplyr::rename(med_val_2020 = 3)
  
  
  # Load HPI data ####
  HPI_county <- readr::read_csv("~/fmv/data/HPIcounty.csv",
                                show_col_types = F)
  
  
  # Generate median home value estimates across 2000-2020 ####
  med_home_value <- 
    HPI_county %>%
    dplyr::mutate(across(HPI_2000:HPI_2020, ~ .x/HPI_2020)) %>%
    dplyr::left_join(homevalue, by = "fips") %>%
    dplyr::mutate(across(HPI_2000:HPI_2020, ~ .x * med_val_2020)) %>%
    dplyr::rename_with(.fn = ~ str_replace(.x, "HPI","VAL"), 
                       .cols = HPI_2000:HPI_2020) %>%
    dplyr::filter(if_any(VAL_2000:VAL_2020, ~ !is.na(.x))) %>%
    tidyr::pivot_longer(
      cols = VAL_2000:VAL_2020,
      names_to = c("prefix","year"),
      names_sep = "_",
      values_to = "MEDHOMEVAL") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::select(fips, year, MEDHOMEVAL)
  
  
  return(med_home_value)
  
}







