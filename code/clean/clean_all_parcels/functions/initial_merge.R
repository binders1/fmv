initial_merge <- function(imported_data) {

  ## Merge sale and crosswalk ####
  
  sample_period <-
    imported_data[['sale_obj']] %>%
    mutate(year = lubridate::year(date)) %>%
    dplyr::filter(year >= 2000 & year <= 2019)
  
  if (nrow(sample_period) == 0) {
    return(NULL)
  }
  
  sample_period %>%
    left_join(imported_data[['salepid_obj']], by = "sid") %>%
    
    # Merge Sales and PCIS ###
    inner_join(
      imported_data[['pcis_obj']],
      by = c("fips", "pid")
      )
  
}
