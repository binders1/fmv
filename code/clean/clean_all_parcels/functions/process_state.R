
process_state.pc <- function(state_index) {
  
  #============================================================
  # 01). Load data, merge, and perform minor cleaning steps
  #============================================================
  
  # Give the index variable pc class for generic method dispatch
  state_index <- structure(state_index, class = "pc")
  
  clean_base <-
    
    # Load sales, sale <-> parcel crosswalk, and processed climate/soil vars
    load_state(state_index = state_index) %>%
    
    # merge all together
    initial_merge() %>% 

    # Add HPI index ####
    clean_HPI.pc()
  
  if (is.null(clean_base)) {
    return(
      message("No observations; moving on\n")
      )
    }
  
  #================================================================
  # 02). Clean and aggregate: irrigation, soil, climate, and others
  #================================================================
  
  # clean and aggregate irrigation variables
  clean_agg_irrigation <- 
    clean_base %>%
    irrFilter.pc()
  
  # clean and aggregate soil variables
  clean_agg_soil <-
    agg_soil.pc(clean_base, state_index = state_index)
  
  # clean and aggregate climate variables 
  clean_agg_climate <-
    agg_climate(clean_base)
  
  #================================================================
  # 03). Merge all cleaned/aggregated datasets together 
  #================================================================
  
  # Create base dataframe on which to join all others
  base_for_join <- 
    clean_base %>%
    select(c(sid, fips, HPI, log_priceadj_ha, 
             date, ha, x, x45, y, y45)) %>%
    filter(!duplicated(sid))
  
  # Create list object of all aggregated datasets
  full_data_for_join <-
    list(
      base_for_join,
      clean_agg_irrigation,
      clean_agg_soil,
      clean_agg_climate,
      clean_agg_mean,
      clean_agg_sum
    )
  
  # Join base data with cleaned/aggregated data
  all_joined <-
    purrr::reduce(
      .x = full_data_for_join,
      .f = left_join,
      by = "sid"
    )
  
  # Add median home values by fips-year
  all_joined %<>%
    mutate(year = year(date)) %>%
    left_join(
      mhv,
      by = c("fips", "year")
    ) %>%
    dplyr::select(!year)
  
  #================================================================
  # 04). Write to disk
  #================================================================
  
  state <- 
    stringr::str_extract(
      pcis_pqt[[state_index]],
      "(?<=_)[A-Z]{2}(?=\\.pqt)"
    )
  
  clean_file <- paste0("clean_", state, ".pqt")
  
  path_to_write <- file.path(clean.dir, clean_file)
  
  arrow::write_parquet(all_joined, 
                       path_to_write)
  
  message("Saved: ", path_to_write)
}
