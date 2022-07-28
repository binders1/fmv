# source: https://www.redfin.com/news/data-center/
redfin_data <- read_tsv(
  gzfile("county_market_tracker.tsv000.gz")
  )



redfin_data %>% glimpse()

redfin_data$state_code %>% unique() %>% sort() %>% length()

redfin_data %>%
  filter(!(state_code %in% c("AK", "HI")))





usmap::countypop %>%
  select(!pop_2015) %>%
  mutate(region = paste0(county, ", ", abbr)) %>%
  pull(region)

redfin_data$region
