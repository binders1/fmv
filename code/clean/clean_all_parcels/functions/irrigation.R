# FUNC: Irrigation Variable Filtering ####  

# Purpose: Creates binary indicators of whether a parcel 
# has been irrigated a) ever, or b) in the last n years

# Args ####

## data: ClimateIrrigationSoil parcel dataframe.

## varname: character string for the derived irrigation variable name.

## years: Optional integer value defining recent irrigation cut-off (3 years).

## replace_na: Logical, indicating whether NA values should be replaced. 

## Defaults to TRUE.

## na_value: Integer value for NA replacement value. Defaults to 0.

## drop_used: Logical, indicating whether irrigation variables used in 
## calculation should be dropped. Defaults to FALSE.

irrFilter <- function(data, varname, years=NULL, replace_na = TRUE, 
                      na_value = 0, drop_used = FALSE) {
  
  tmp <- data %>%
    dplyr::select(pid, year, starts_with("Irrigation")) %>%
    tidyr::pivot_longer(
      cols = Irrigation1997:Irrigation2017,
      names_to = "irr_date",
      values_to = "irr_status"
    ) %>%
    dplyr::mutate(
      irr_date = as.numeric(stringr::str_remove_all(irr_date, "Irrigation")),
      diff = year - irr_date
    ) 
  
  if(!is.null(years)) {
    tmp <- tmp %>%
      dplyr::filter(diff <= years)
  }
  
  tmp1 <- tmp %>%
    dplyr::mutate(irr_status = ifelse(diff>=0, irr_status, 0)) %>%
    dplyr::group_by(pid) %>%
    dplyr::filter(irr_status == max(irr_status, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(diff >= 0) %>%
    dplyr::filter(!duplicated(pid)) %>%
    dplyr::rename(!!varname := "irr_status") %>%
    dplyr::ungroup() %>%
    dplyr::select(pid, year, !!varname)
  
  out <- data %>%
    dplyr::left_join(tmp1, by = c("year", "pid"))
  
  if(replace_na) {
    
    out[[varname]] <- tidyr::replace_na(out[[varname]],na_value)
    
  }
  
  out[[varname]] <- ifelse(is.na(out$Irrigation1997),
                           NA,
                           out[[varname]])
  
  if(drop_used) {
    
    out <- out %>%
      dplyr::select(!dplyr::starts_with('Irrigation'))
    
  }
  
  out
  
}



agg_irrigation <- function(data) {
  
  data %>%
    irrFilter('irrEver') %>%
    irrFilter('irrRecent', years = 3,
              drop_used = TRUE) %>%
    suppressWarnings() %>%
    group_by(sid) %>%
    summarise(across(starts_with('irr'), max)) %>%
    ungroup()
  
}









