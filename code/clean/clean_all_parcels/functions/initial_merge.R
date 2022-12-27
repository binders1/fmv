initial_merge <- function(imported_data) {
  UseMethod("initial_merge")
}

initial_merge.pc <- function(imported_data) {

  imported_data[['pc_obj']] %>%
    
    # Create fips variable
    mutate(fips = stringr::str_sub(pid, 1, 5)) %>%
    
    # Merge parcels and PCIS ###
    left_join(
      imported_data[['pcis_obj']],
      by = c("fips", "pid")
      )

}
