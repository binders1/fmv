
noCoast <- function() {
  
  setwd("~/fmv/data/cleaned")
  
  state_df <- usmap::countypop %>%
    dplyr::mutate(state = str_sub(fips, 1, 2)) %>%
    dplyr::select(state, abbr) %>%
    distinct() %>%
    filter(!is.element(abbr, c("AK", "HI")))
  
  
  
  
  cst_present <- map(all_clean, 
                     ~ read_parquet(.x) %>%
                       names() %>%
                       str_detect("cst_") %>%
                       sum())
  
  
  names(cst_present) <- str_extract(all_clean, "[:upper:]{2}")
  
  
  cst_df <- tibble(abbr = names(cst_present), cst = unlist(cst_present))
  
  no_cst_states <- cst_df %>%
    left_join(state_df, by = "abbr") %>%
    filter(cst == 0) %>%
    pull(state)
  
  return(no_cst_states)
  
}



