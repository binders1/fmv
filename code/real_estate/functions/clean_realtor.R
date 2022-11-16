
# source: https://www.realtor.com/research/data/

clean_realtor <- function() {
  
  # Load data ####
  realtor_data <- 
    read_parquet(
      file.path(mhv.dir, "realtor/realtor_listing.pqt")
    )
  
  realtor_data %<>%
    rename(date = "month_date_yyyymm") %>%
    filter(
      # remove Alaska, Hawaii, and Marshall Islands
      !(str_detect(county_name, "(ak|hi|mh)$")),
      # Remove data quality note from bottom row
      str_detect(date, "^[:digit:]{6}$")
    )
  
  # Parse dates ####
  realtor_data %<>%
    mutate(date = ym(date),
           year = year(date),
           month = month(date)) %>%
    relocate(date, year, month)
  
  
  # Clean fips codes ####
  realtor_data %<>%
    mutate(fips = case_when(
      nchar(county_fips) == 4 ~ paste0("0",county_fips),
      TRUE ~ as.character(county_fips)
    ), 
    .keep = "unused"
    ) %>%
    relocate(fips, .after = "month")
  
  # Select median listing price ####
  realtor_data %<>%
    select(date, year, month, fips, 
           median_listing_price,
           quality_flag)
  
  # Replace quality_flag NA's with 0
  realtor_data %<>%
    mutate(quality_flag = replace_na(quality_flag, 0))
  
  
  # Summarise county-level median_listing_price from 2017,
  # year with best quality_flag good:bad ratio
  realtor_data %>%
    filter(year == 2017) %>%
    group_by(fips) %>%
    summarise(
      median_listing_price = mean(median_listing_price)
    ) %>%
    mutate(year = 2017)
  
}
