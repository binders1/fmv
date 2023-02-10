
read_frr_data <- 
  function(
    frr = c("Heartland", "Northern Crescent", "Northern Great Plains",
            "Prairie Gateway","Eastern Uplands", "Southern Seaboard",
            "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
    only.nolte.counties = FALSE
    ) {
  
    frr <- match.arg(frr)
    
    # States that overlap with FRR
    states_to_load <-
      county_frr_crosswalk %>%
      filter(frr_name == frr) %>%
      pull(stusps) %>%
      unique()
    
    # Counties contained within FRR
    frr_counties <-
      county_frr_crosswalk %>%
      filter(
        frr_name == frr,
        # Restrict to only ncb counties, if specified in fn call
        if (only.nolte.counties) fips %in% nolte_counties else TRUE
        ) %>%
      pull(fips) %>%
      sort()
    
    # Return FRR data
    map_dfr(
      .x = states_to_load, 
      .f = filter_to_frr_counties,
      frr_counties = frr_counties
    )
  }


filter_to_frr_counties <- function(state, frr_counties) {
  read_state_clean(state) %>%
    filter(fips %in% frr_counties)
}
