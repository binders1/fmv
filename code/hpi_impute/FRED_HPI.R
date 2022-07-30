library(fredr)

Sys.setenv(FRED_API_KEY = "db828b951775e7f2dc8cc3c88541a117")


## Home Price Index by County ####

HPI_1 <- fredr_release_series(171, 
                              offset = 0,
                              realtime_start = as_date('2000-01-01'),
                              realtime_end = as_date('2020-01-01'))

HPI_2 <- fredr_release_series(171, 
                              offset = nrow(HPI_1),
                              realtime_start = as_date('2000-01-01'),
                              realtime_end = as_date('2020-01-01'))

HPI_3 <- fredr_release_series(171, 
                              offset = nrow(HPI_1)+nrow(HPI_2),
                              realtime_start = as_date('2000-01-01'),
                              realtime_end = as_date('2020-01-01'))

HPI_county <- bind_rows(HPI_1, HPI_2, HPI_3) %>% 
  
  filter(str_detect(title, regex("county|borough|parish", ignore_case = T))) %>%
  filter(!str_detect(title, "MSA")) %>%
  
  mutate(fips = str_extract(id, "(?<=ATNHPIUS).{5}"),
         county_name = str_extract(title, "(?<=Index for[:blank:]).+[:blank:](County|Borough|Parish)"),
         state = str_extract(title, "(?<=County,[:blank:]).{2}"),
         realtime_start = as_date(realtime_start)) %>%
  
  filter(!duplicated(fips))


base_HPI <- tibble(year_id = paste0("HPI_", seq(2000, 2020)))

HPI_tbl <- tibble(names = c('fips', paste0("HPI_", seq(2000, 2020)))) %>%
  mutate(na = NA) %>%
  pivot_wider(
    names_from = names,
    values_from = na
  ) %>%
  slice(0)

for (i in seq_len(nrow(HPI_county))) {
  
  tmp <- fredr(
    series_id = HPI_county$id[[i]],
    observation_start = as.Date("2000-01-01"),
    observation_end = as.Date("2020-01-01")
  ) %>%
    mutate(year_id = paste0("HPI_", year(date))) %>%
    right_join(base_HPI) %>% 
    arrange(year_id) %>%
    dplyr::select(year_id,value) %>% 
    mutate(fips = HPI_county$fips[[i]]) %>%
    pivot_wider(
      names_from = year_id,
      values_from = value
    ) %>%
    suppressMessages()
  
  HPI_tbl <- rbind(HPI_tbl, tmp)
  
  cat("Completed: ", HPI_county$county_name[[i]], ", ", HPI_county$state[[i]],
      " |.......| ", round(i/nrow(HPI_county)*100,2), "%\n",
      sep = "")
  
}



