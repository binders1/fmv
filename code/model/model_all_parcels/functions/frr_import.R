

frr_import <- function(frr_id, buildings = TRUE, dir = c("cleaned", "all_parcels")) {
  
  # Determine directory from which load cleaned data
  dir <- match.arg(dir)
  
  target_dir <- 
    switch(dir,
           cleaned = clean_dir,
           all_parcels = clean_pc_dir)
  
  prefix <- 
    switch(dir,
           cleaned = "clean_",
           all_parcels = "clean_all_pc_")
  
  frr_name <- 
    frr_key %>%
    filter(id == frr_id) %>%
    pull(frr_name)
  
  if (buildings) {
    cli::cli_h1(paste0("Trying: ", frr_name, " with buildings"))
    
  } else {
    cli::cli_h1(paste0("Trying: ", frr_name, " without buildings"))
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
    split(.$stusps) %>%
    map(~ pull(.x, fips))
  
  clean_to_load <- 
    paste0(prefix,
           states_to_load,
           ".pqt") %>%
    sort()
  
  ## Import current FRR dataframe ####
  
  cli::cli_alert_info(
    paste0(states_to_load, collapse = ", "))
  
  cli::cli_alert_info(
    paste0(map_int(counties_to_include, length) %>% sum(), 
           " counties")
  )
  
  clean_paths <- file.path(target_dir, clean_to_load)
  
  df_import <-
    map2_dfr(
      .x = clean_paths,
      .y = counties_to_include,
      # filter to only current frr counties
      .f = ~ frr_fips_filter(.x, .y)
      ) %>%
    
    mutate(
      # Replace NA soil values with 0
      across(.cols = any_of(soil_vars), .fns = ~ replace_na(.x, 0)),
      # Create state variable
      state = str_sub(fips, 1, 2)
      )
  
  # Remove parcels with buildings for building-free analysis
  if (!buildings) {
    df_import <- 
      df_import %>%
      dplyr::filter(p_bld_fp == 0)
  }
  
  imported_vars <- 
    paste(names(df_import), collapse = ", ")
  
  ## Replace NA cst_* with 0 for land-locked states
  
  model_df <-
    if (str_detect(imported_vars, "cst_")) {
      df_import %>%
        mutate(
          across(
            .cols = starts_with("cst"),
            .fns = ~ if_else((state %in% no_cst_states) & is.na(.x), 0, .x)
            )
          )
  } else {
    df_import %>%
      mutate(cst_2500 = 0, cst_50 = 0)
  }
  
  # Drop NAs
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


frr_fips_filter <- function(file, counties) {
  
  read_parquet(file) %>%
    dplyr::filter(fips %in% counties)
}


