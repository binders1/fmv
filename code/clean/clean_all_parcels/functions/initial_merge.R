
fips_extract <- function(data) {
  data %>%
    dplyr::mutate(fips = stringr::str_sub(pid, 1, 5)) %>%
    dplyr::relocate(pid, fips)
}



initial_merge <- function(imported_data) {
  UseMethod("initial_merge")
}

initial_merge.pc <- function(imported_data) {
  
  # Add FIPs variable to each dataset
  data_with_fips <-
    map(
      imported_data,
      fips_extract
      )
  
  # Merge parcel with PCIS data
  data_with_fips[['pc_obj']] %>%
    
    # Merge parcels and PCIS ###
    left_join(
      data_with_fips[['pcis_obj']],
      by = c("fips", "pid")
      )

}
