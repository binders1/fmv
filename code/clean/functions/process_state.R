
# Parent cleaning function. Processes a given state's data:
# 
#' 1) Merge sales and parcel observations into a single dataframe,
#' 2) Convert inflation metrics to base year 2020 and scaling the 
#'    sale price indicator accordingly
#' 3) Merge HPI panel 
#' 4) Convert sale price to log($/ha)
#' 5) Create parcel-level irrigation indicators: ever irrigated, and irrigated in
#'    past 3 years
#' 6) Determine proportion of sales area taken up by each soil type
#' 7) Aggregate climate variables by meterological season at the parcel level, then
#'    aggregate to sale-level using the ha-weighted average across all parcels 
#'    within the sale
#' 8) Aggregate parcel-level variables from Nolte (2020) to the sale level, 
#'    either averaging or summing depending on what was determined appropriate

process_state <- function(state_index) {
  
  #============================================================
  # 01). Load data, merge, and perform minor cleaning steps
  #============================================================
  
  clean_base <-
    
    # Load sales, sale <-> parcel crosswalk, and processed climate/soil vars
    load_state(state_index = state_index) %>%
    
    # merge all together
    initial_merge() %>% 
    
    # Note: see functions/intial_clean.R for all cleaning_*() functions
    
    # perform inflation adjustments 
    clean_inflation() %>%
    
    # Add HPI index ####
    clean_HPI() %>%
    
    # Create logged $/ha
    clean_logprice()
  
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
    agg_irrigation(clean_base)
  
  # clean and aggregate soil variables
  clean_agg_soil <-
    agg_soil(clean_base, 
             state_index = state_index)
  
  # clean and aggregate climate variables 
  clean_agg_climate <-
    agg_climate(clean_base)
  
  # agg_mean
  clean_agg_mean <- 
    agg_by_method(
      clean_base,
      noltevars_to_mean,
      mean
    )
  
  # agg_sum
  clean_agg_sum <-
    agg_by_method(
      clean_base, 
      noltevars_to_sum,
      sum
    )
  
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
