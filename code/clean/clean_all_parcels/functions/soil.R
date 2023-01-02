# Load state-specific soilcodes
load_soilcodes <- function(state_index) {

  code_to_load <- 
    list.files(soil_dir)[state_index] %>%
    file.path(soil_dir, .)
  
  soilcodes <- 
    readr::read_csv(code_to_load, show_col_types = FALSE) %>%
    left_join(soil_crosswalk, by = "farmlndcl") %>%
    dplyr::mutate(Value = paste0("VALUE_", Value))
  
  pattern_to_drop <- 
    stringr::regex("(not prime|missing)", 
                   ignore_case = TRUE)
  
  soil_to_drop <- 
    soilcodes %>%
    dplyr::filter(
      stringr::str_detect(farmlndcl, pattern_to_drop)
      ) %>%
    dplyr::pull(Value) %>%
    str_c(., "_prop")
  
  soil_ref_tbl <- 
    soilcodes %>%
    dplyr::filter(
      !stringr::str_detect(farmlndcl, pattern_to_drop)
      ) %>%
    group_by(Value) %>%
    summarise(category = paste0(category, collapse = "; ")) %>%
    mutate(Value = paste0(Value, "_prop"))
  
  
  list(
    soil_to_drop = soil_to_drop,
    soil_ref_tbl = soil_ref_tbl
  )
  
}

# Clean and aggregate soil data within parcel

agg_soil.pc <- function(data, state_index) {
  
  # unpack soilcodes
  soilcodes <- load_soilcodes(state_index)
  soil_to_drop <- soilcodes[['soil_to_drop']]
  soil_ref_tbl <- soilcodes[['soil_ref_tbl']]
  
  # Calculate total soil area in parcel
  calc_soil_area <- 
    data %>%
    select(pid, starts_with("VALUE")) %>%
    pivot_longer(
      cols = starts_with("VALUE"),
      names_to = "type",
      values_to = "soil_area") %>%
    group_by(pid) %>%
    summarise(total_soil_area = sum(soil_area)) %>%
    ungroup() %>%
    select(pid, total_soil_area) 
  
   
  soil_ha_to_m2 <- 
    data %>%
    dplyr::select(!any_of(soil_to_drop)) %>%
    left_join(calc_soil_area, by = "pid") %>%
    
    # convert sq meters to hectares
    mutate(
      across(
        c(starts_with("VALUE"), total_soil_area), 
        ~ .x * 1e-04
        )
      )
  
  ## Calculate proportion of each soil type in a parcel ####
  
  soil_pc_prop <-
    soil_ha_to_m2 %>%
    mutate(
      across(
        starts_with('VALUE'),
        ~ .x / total_soil_area, .names = "{.col}_prop"
        )
      ) %>%
    select(pid, ends_with("prop")) %>%
    mutate(across(starts_with("VALUE"), ~ replace_na(.x, 0))) %>%   
    dplyr::select(!any_of(soil_to_drop))
  
  # replace VALUE_* with descriptive names
  soil_type_names <-
    soil_pc_prop %>%
    pivot_longer(
      cols = starts_with("VALUE"),
      names_to = "Value",
      values_to = "prop"
    ) %>% 
    left_join(soil_ref_tbl, by = "Value") %>%
    dplyr::select(!Value)
  
  # Aggregate duplicate soil category within parcel
  soil_type_agg <-
    soil_type_names %>%
    separate_rows(category, sep = "; ") %>%
    group_by(pid, category) %>%
    summarise(prop = sum(prop), .groups = "keep") %>%
    pivot_wider(
      names_from = category,
      values_from = prop
    )
  
  soil_type_agg
  
}
