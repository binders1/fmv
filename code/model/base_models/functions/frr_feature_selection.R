frr_feature_selection <- function(frr_data, model_predictors) {
  
  # Select only model predictors
  frr_select_predictors <-
    frr_data %>%
    select(any_of(model_predictors)) %>%
    mutate(state = str_sub(fips, 1, 2))
  

  coast_variable_present <- any(grepl("cst_", names(frr_data)))

  frr_coast_imputed <-
    if (coast_variable_present) {
      # In FRRs that touch water, replace NA cst_* with 0 for land-locked states 
      frr_data %>%
        mutate(
          across(
            .cols = c(cst_2500, cst_50),
            .fns = ~ if_else(state %in% no_cst_states & is.na(.x), 0, .x)
            )
          )
      } else {
      # In land-locked FRRs, create coast variables and 
      frr_data %>%
        mutate(cst_2500 = 0, cst_50 = 0)
      }

  
  frr_coast_imputed %>%
    # Join Median Home Values by fips-year
    mutate(year = lubridate::year(date)) %>%
    left_join(
      medhomeval, by = c("fips", "year")
    ) %>%
    # Remove non-predictor helper columns
    select(-c(fips, year, state)) %>%
    # Remove NA rows
    na.omit()
  
  }