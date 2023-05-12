
# Basic data cleaning of Realtor.com MHV data. This function filters to the year
# specified in the mhv_year argument, to provide a "base" for the later annual MHV 
# imputation using annual county HPI values

# Data source: https://www.realtor.com/research/data/

clean_realtor <- function(mhv_year) {
  
  stopifnot(is.numeric(mhv_year))
  stopifnot(mhv_year %in% seq(2016, 2022))
  
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
  
  # Summarise county-level median_listing_price from a given year,
  # 2017 in the original, as it is the year with best quality_flag good:bad ratio
  # 2020 for pred_all_parcels
  realtor_data %>%
    filter(year == mhv_year) %>%
    group_by(fips) %>%
    summarise(
      median_listing_price = mean(median_listing_price),
      year = unique(year)
      ) 
}
