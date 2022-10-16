# Load state-specific soilcodes
load_soilcodes <- function(state_index) {

  code_to_load <- 
    list.files(soil_dir)[state_index] %>%
    file.path(soil_dir, .)
  
  soilcodes <- 
    readr::read_csv(
      code_to_load,
      show_col_types = FALSE
    ) %>%
    
    left_join(soil_crosswalk, 
              by = "farmlndcl") %>%
    dplyr::mutate(Value = paste0("VALUE_", Value))
  
  pattern_to_drop <- 
    stringr::regex("(not prime|missing)", 
                   ignore_case = TRUE)
  
  soil_to_drop <- 
    soilcodes %>%
    dplyr::filter(
      stringr::str_detect(farmlndcl,
                          pattern_to_drop)
      ) %>%
    dplyr::pull(Value) %>%
    str_c(., "_prop")
  
  soil_ref_tbl <- 
    soilcodes %>%
    dplyr::filter(
      !stringr::str_detect(farmlndcl,
                           pattern_to_drop)
      ) %>%
    group_by(Value) %>%
    summarise(category = paste0(category, collapse = "; ")) %>%
    mutate(Value = paste0(Value, "_prop"))
  
  
  list(
    soil_to_drop = soil_to_drop,
    soil_ref_tbl = soil_ref_tbl
  )
  
}

# Clean and aggregate soil data

agg_soil <- function(data, state_index) {
  
  # unpack soilcodes
  soilcodes <- load_soilcodes(state_index)
  soil_to_drop <- soilcodes[['soil_to_drop']]
  soil_ref_tbl <- soilcodes[['soil_ref_tbl']]
  
  # Calculate total soil area in sales record
  df_soil_tmp <- 
    data %>%
    pivot_longer(
      cols = starts_with("VALUE"),
      names_to = "type",
      values_to = "soil_area") %>%
    group_by(sid) %>%
    summarise(total_soil_area = sum(soil_area)) %>%
    ungroup() %>%
    select(sid, total_soil_area) 
  
  # Convert sq meters to hectares 
  df_final_soil <- 
    data %>%
    dplyr::select(!any_of(soil_to_drop)) %>%
    left_join(df_soil_tmp, by = "sid") %>%
    # convert sq meters to hectares
    mutate(
      across(c(starts_with("VALUE"), total_soil_area),
             ~ .x * 1e-04)
      )
  
  ## Aggregate across parcels in single sale ####
  
  df_final_soil %>%
    group_by(sid) %>%
    summarise(across(c(starts_with('VALUE'), total_soil_area), sum)) %>%
    ungroup() %>%
    mutate(across(starts_with('VALUE'), 
                  ~ .x / total_soil_area, .names = "{.col}_prop")) %>%
    select(sid, ends_with("prop")) %>%
    mutate(across(starts_with("VALUE"), ~ replace_na(.x, 0))) %>%   
    dplyr::select(!any_of(soil_to_drop)) %>%
    pivot_longer(
      cols = starts_with("VALUE"),
      names_to = "Value",
      values_to = "prop"
    ) %>% 
    left_join(soil_ref_tbl, by = "Value") %>%
    dplyr::select(!Value) %>%
    separate_rows(category, sep = "; ") %>%
    group_by(sid, category) %>%
    summarise(prop = sum(prop), .groups = "keep") %>%
    pivot_wider(
      names_from = category,
      values_from = prop
    )
  
}
