# irrFilter.pc: Check if parcel has ever been irrigated ####  
# Args ####

## data: ClimateIrrigationSoil parcel dataframe.

irrFilter.pc <- function(data) {
  
  irr_ever_df <-
    data %>%
    
    dplyr::select(pid, starts_with("Irrigation")) %>%
    
    tidyr::pivot_longer(
      cols = Irrigation1997:Irrigation2017,
      names_to = "irr_year",
      values_to = "irrEver"
    ) %>%
    
    # Determine if parcel has ever been irrigated
    dplyr::group_by(pid) %>%
    
    dplyr::mutate(
      # Impute 0 to parcels with no irrigation information
      entire_pid_na = all(is.na(irrEver)),
      irrEver = if_else(entire_pid_na, 0, irrEver)
    ) %>%
    
    dplyr::filter(irrEver == max(irrEver, na.rm = TRUE)) %>%
    
    dplyr::ungroup() %>%
    
    dplyr::filter(!duplicated(pid)) %>%
    
    dplyr::select(pid, irrEver)
  
  # Rejoin to original input data
  data %>%
    dplyr::left_join(irr_ever_df, by = "pid")
  
}




