frr_feature_selection <- function(frr_data, model_predictors) {
  
  # Select only model predictors
  frr_select_predictors <-
    frr_data %>%
    select(any_of(model_predictors)) %>%
    mutate(state = str_sub(fips, 1, 2))

  # -- Coastal Clean-up Explained ----------------------------------------------
  # Some FRRs are land-locked -> those should have 0 values for all coastal vars
  # Some FRRs touch water, consisting of some land-locked states and some not ->
  # those should have 0 values for coastal vars in land-locked states, and leave 
  # coastline vars alone in states that touch water
  # -------------------------------------------------------------------------- #
  
  coast_variable_present <- any(grepl("cst_", names(frr_data)))

  frr_coast_imputed <-
    if (coast_variable_present) {
      # In FRRs that touch water, replace NA cst_* with 0 for land-locked states 
      frr_select_predictors %>%
        mutate(
          across(
            .cols = c(cst_2500, cst_50),
            .fns = ~ if_else(state %in% no_cst_states & is.na(.x), 0, .x)
            )
          )
      } else {
      # In land-locked FRRs, create coast variables and assign all values 0
        frr_select_predictors %>%
          mutate(cst_2500 = 0, cst_50 = 0)
      }

  frr_coast_imputed %>%
    mutate(year = lubridate::year(date)) %>%
    # Remove non-predictor helper columns ====================
    select(-c(fips, year, state))
    # UNECESSARY ---> Join median home values by fips-year 
    # UNECESSARY ---> left_join(medhomeval, by = c("fips", "year")) %>%
    
  
  }