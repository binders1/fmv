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



## Actual Median House Prices ####

library(censusapi)
apis <- listCensusApis()
View(apis)

### Set API key ####
Sys.setenv(CENSUS_KEY="24a0e6e31ba71d8b3e0f70ba0b4037fd194d6aec")


metadata <- listCensusMetadata(
  name = "2020/acs/acs5",
  type = "variables"
)

metadata %>% 
  filter(str_detect(name, "B25077")) %>%
  View()

HPI_county <- readr::read_csv("~/fmv/data/HPIcounty.csv",
                              show_col_types = F)

  
homevalue <- 
  getCensus(name = "acs/acs5",
            vintage = 2020, 
            vars = "B25077_001E", 
            region = "county:*") %>% 
  filter(!is.element(state, c('02','15','72'))) %>%
  mutate(fips = paste0(state,county)) %>%
  rename(med_val_2020 = 3)

med_home_value <- 
  HPI_county %>%
  mutate(across(HPI_2000:HPI_2020, ~ .x/HPI_2020)) %>%
  left_join(homevalue, by = "fips") %>%
  mutate(across(HPI_2000:HPI_2020, ~ .x * med_val_2020)) %>%
  rename_with(.fn = ~ str_replace(.x, "HPI","VAL"), 
              .cols = HPI_2000:HPI_2020) %>%
  filter(if_any(VAL_2000:VAL_2020, ~ !is.na(.x))) %>%
  pivot_longer(
    cols = VAL_2000:VAL_2020,
    names_to = c("prefix","year"),
    names_sep = "_",
    values_to = "MEDHOMEVAL") %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(fips, year, MEDHOMEVAL)

## VIZ ####

med_home_value %>%
  
  ggplot(aes(year, MEDHOMEVAL)) +
  
  geom_boxplot(aes(group = year),
               colour = "grey30",
               outlier.colour = "#1696d2", 
               alpha = 0.3) + 
  
  scale_y_continuous(trans = "log10",
                     labels = scales::comma) +

  scale_x_continuous(breaks = c(2000,
                               2005,
                               2010,
                               2015,
                               2020)) +
  
  labs(title = "Median Home Value Over Time",
       subtitle = "County-level observations",
       caption = "*CPI-adjusted 2020 dollars",
       y= "Median Home Value ($)*",
       x = NULL) +
  
  theme(
    text = element_text(family = "Source Sans Pro", size = 25),
    axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", 
                                    size = 0.3),
    panel.grid.minor.y = element_line(colour = "lightgrey",
                                      size = 0.2),
    panel.border = element_rect(fill = NA, colour = "grey30",
                                size = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(face = "italic", size = 15),
    plot.margin = margin(rep(10,4))
  )







