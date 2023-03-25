
process_state.pc <- function(state_index) {
  
  #============================================================
  # 01). Load data, merge, and perform minor cleaning steps
  #============================================================
  
  tic("clean_base")
  clean_base <-
    
    # Load sales, sale <-> parcel crosswalk, and processed climate/soil vars
    load_state.pc(state_index) %>%
    # merge all together
    initial_merge() %>% 
    # Add HPI index ####
    clean_HPI.pc() 
  toc()
  
  if (is.null(clean_base)) {
    return(
      message("No observations; moving on\n")
      )
    }
  
  #================================================================
  # 02). Clean and aggregate: irrigation, soil, climate, and others
  #================================================================
  
  tic("irrFilter.pc()")
  # clean and aggregate irrigation variables
  clean_agg_irrigation <- 
    clean_base %>%
    irrFilter.pc()
  toc()
  
  tic("agg_soil.pc()")
  # clean and aggregate soil variables
  clean_agg_soil <-
    clean_base %>%
    agg_soil.pc(state_index = state_index)
  toc()
  
  tic("agg_climate.pc()")
  # clean and aggregate climate variables 
  clean_agg_climate <-
    clean_base %>%
    agg_climate.pc()
  toc()
  
  #================================================================
  # 03). Merge all cleaned/aggregated datasets together 
  #================================================================
  
  tic("base_for_join")
  # Create base dataframe on which to join all others
  base_for_join <- 
    clean_base %>%
    select(pid, fips, HPI, ha, x, x45, y, y45) %>%
    filter(!duplicated(pid))
  toc()
  
  
  # Create list object of all aggregated datasets
  full_data_for_join <-
    list(
      base_for_join,
      clean_agg_irrigation,
      clean_agg_soil,
      clean_agg_climate
    )
  
  
  tic("all_joined")
  all_joined <-
    
    # Join base data with cleaned/aggregated data
    purrr::reduce(
      .x = full_data_for_join,
      .f = left_join,
      by = "pid"
    ) %>%
    
    # Add average county median home values by fips (no time dimension for pred_all_parcels)
    left_join(
      mhv_mean,
      by = "fips"
    )
  toc()
  
  #================================================================
  # 04). Write to disk
  #================================================================
  
  state <- 
    stringr::str_extract(
      pcis_pqt[[state_index]],
      "(?<=_)[A-Z]{2}(?=\\.pqt)"
    )
  
  clean_file <- paste0("clean_all_pc_", state, ".pqt")
  
  path_to_write <- file.path(pc_clean.dir, clean_file)
  
  arrow::write_parquet(all_joined, 
                       path_to_write)
  
  message("Saved: ", path_to_write)
  
}
