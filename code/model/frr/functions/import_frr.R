
frr_import <- function(frr_id) {
  
  frr_name <- 
    frr_key %>%
    filter(id == frr_id) %>%
    pull(frr_name)
  
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
  
  clean_paths <- file.path(clean_dir, clean_to_load)
  
  df_import <- 
    map_dfr(clean_to_load, read_parquet) %>%
    
    # filter to only current frr counties
    filter(fips %in% counties_to_include) %>%
    mutate(across(.cols = any_of(soil_vars),
                  .fns = ~ replace_na(.x, 0))) %>%
    mutate(state = str_sub(fips, 1, 2))
  
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
  
  model_df %>%
    select(!c(fips, state, HPI)) %>%
    na.omit()
  
}