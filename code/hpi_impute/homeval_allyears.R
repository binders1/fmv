
setwd("~/fmv/data/homevalue")


years_to_fetch <- seq(2010, 2020)

for (year in years_to_fetch) {
  
  homevalue <- censusapi::getCensus(name = "acs/acs5",
                                    vintage = year, 
                                    vars = "B25077_001E", 
                                    region = "county:*") %>%
    tibble() %>% 
    dplyr::filter(!is.element(state, c('02','15','72'))) %>%
    transmute(fips = paste0(state, county), MEDHOMEVAL = B25077_001E,
              year = year)
  
  
  file <- paste0("medhomeval_", year, ".pqt")
  
  write_parquet(homevalue,
                file)
  
}