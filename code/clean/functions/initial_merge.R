initial_merge <- function(imported_data) {

  ## Merge sale and crosswalk ####
  
  imported_data[['sale_obj']] %>%
    
    mutate(year = lubridate::year(date)) %>%
    dplyr::filter(year >= 2000 & year <= 2019) %>%
    
    left_join(imported_data[['salepid_obj']], by = "sid") %>%
    
    # Merge Sales and PCIS ###
    inner_join(
      imported_data[['pcis_obj']],
      by = c("fips", "pid")
      )
  
}
