frr_import <- function(frr_id, buildings = TRUE) {
  
  frr_name <- 
    frr_key %>%
    filter(id == frr_id) %>%
    pull(frr_name)
  
  if (buildings) {
    cli::cli_h1(paste0("Trying: ", frr_name, " with buildings"))
    
  } else {
    cli::cli_h1(paste0("Trying: ", frr_name))
  }
  
  cli::cli_h2("Loading Data")
  
  ## Specify states to load in ####
  states_to_load <- 
    county_frr_crosswalk %>%
    filter(id == frr_id) %>%
    pull(stusps) %>%
    unique()
  
  ## Filter counties ####
  counties_to_include <- 
    county_frr_crosswalk %>%
    filter(id == frr_id) %>%
    pull(fips) %>%
    sort()
  
  clean_to_load <- 
    paste0("clean_",
           states_to_load,
           ".pqt") %>%
    sort()
  
  ## Import current FRR dataframe ####
  
  cli::cli_alert_info(
    paste0(str_extract(clean_to_load, "[:upper:]{2}"), collapse = ", "))
  
  cli::cli_alert_info(
    paste0(length(counties_to_include), " counties")
  )
  
  clean_paths <- file.path(clean_dir, clean_to_load)
  
  df_import <- 
    map_dfr(clean_to_load, read_parquet) %>%
    
    # filter to only current frr counties
    filter(fips %in% counties_to_include) %>%
    mutate(across(.cols = any_of(soil_vars),
                  .fns = ~ replace_na(.x, 0))) %>%
    mutate(state = str_sub(fips, 1, 2))
  
  # Remove parcels with buildings for building-free analysis
  if (!buildings) {
    df_import <- 
      df_import %>%
      dplyr::filter(p_bld_fp == 0)
  }
  
  imported_vars <- 
    paste(names(df_import), collapse = ", ")
  
  ## Replace NA cst_* with 0 for land-locked states
  
  if (str_detect(imported_vars, "cst_")) {
    
    model_df <- 
      df_import %>%
      mutate(across(.cols = starts_with("cst"),
                    .fns = ~ case_when(
                      state %in% no_cst_states & is.na(.x) ~ 0,
                      TRUE ~ .x)
      )
      )
  } else {
    
    model_df <- 
      df_import %>%
      mutate(cst_2500 = 0,
             cst_50 = 0)
  }
  
  out <-
    model_df %>%
    select(!c(fips, state, HPI)) %>%
    na.omit()
  
  cli::cli_alert_info(
    paste0("After filtering: ",
           scales::comma(nrow(out)),
           " obervations")
  )
  
  out
  
  
}
