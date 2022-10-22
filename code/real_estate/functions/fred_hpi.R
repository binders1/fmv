
# ========================================================
# Retrieve 2000-2020 HPI for all (available) 
# counties from FRED API
# ========================================================

fred_hpi <- function() {
  
  # Home Price Index by County ####
  
  # Retrieve metadata on county HPI availability
  HPI_1 <- fredr_release_series(171, 
                                offset = 0)
  
  HPI_2 <- fredr_release_series(171, 
                                offset = nrow(HPI_1))
  
  HPI_3 <- fredr_release_series(171, 
                                offset = nrow(HPI_1)+nrow(HPI_2))
  
  counties_with_hpi <- 
    
    # collate metadata
    bind_rows(HPI_1, HPI_2, HPI_3) %>% 
    
    # filter to only county-equivalent entities
    filter(str_detect(title, 
                      regex("county|borough|parish", 
                            ignore_case = T)),
           !str_detect(title, "MSA")
    ) %>%
    
    # extract county fips and name
    mutate(fips = str_extract(id, "(?<=ATNHPIUS).{5}"),
           county_name = 
             str_extract(title,
                         "(?<=Index for[:blank:]).+[:blank:](County|Borough|Parish)"
             ),
           state = str_extract(title, "(?<=County,[:blank:]).{2}"),
           realtime_start = as_date(realtime_start)) %>%
    
    # remove duplicates
    filter(!duplicated(fips)) %>%
    
    select(
      fred_county_id = id,
      fred_county_fips = fips
    )
  
  # create 2000-2020 reference table to fill out missing years in data
  base_HPI_years <- 
    tibble(year = seq(2000, 2020))
  
  get_county_hpi <- function(fred_county_id, fred_county_fips) {
    
    hpi <- 
      
      # fetch county observations
      fredr(
        series_id = fred_county_id,
        observation_start = as.Date("2000-01-01"),
        observation_end = as.Date("2020-01-01")
      ) %>%
      
      mutate(year = year(date)) %>%
      
      # fill out any missing years
      right_join(base_HPI_years, by = "year") %>% 
      
      arrange(year) %>%
      dplyr::select(year, hpi = value) %>% 
      mutate(fips = fred_county_fips) %>%
      relocate(fips)
    
    message("Completed: ", fred_county_fips, "\n")
    
    invisible(hpi)
    
  }
  
  pmap_dfr(
    counties_with_hpi,
    get_county_hpi
    ) %>%
    pivot_wider(
      names_from = year,
      values_from = hpi,
      names_prefix = "HPI_"
    ) 
  
}

