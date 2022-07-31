all_clean <- list.files('~/fmv/data/cleaned')


missing_hpi_national <- 
  map_dfr(all_clean, read_parquet) %>% 
  dplyr::select(fips, HPI) %>%
  mutate(HPI_present = ifelse(!is.na(HPI), 1, 0)) %>%
  group_by(fips) %>%
  summarise(n = n(),
    HPI_count = sum(HPI_present)) %>%
  transmute(fips, 
            n_sales = n,
            HPI_binary = ifelse(HPI_count > 0, 1, 0))


lost_sales <- missing_hpi_national %>% 
  group_by(HPI_binary) %>% 
  summarise(n_sales = sum(n_sales)) %>% 
  filter(HPI_binary==0) %>%
  dplyr::select(n_sales) %>%
  pull() %>%
  scales::comma()


percent_lost <- missing_hpi_national %>% 
  group_by(HPI_binary) %>% 
  summarise(n_sales = sum(n_sales)) %>%
  ungroup() %>%
  summarise(percent_lost = n_sales/sum(n_sales)) %>%
  pull() %>%
  scales::label_percent(0.01)() %>%
  .[1]


missing_hpi_national %>%
  mutate(HPI_binary = case_when(
    HPI_binary == 1 ~ "Present",
    TRUE ~ "Missing"
  )) %>%
  
  usmap::plot_usmap(
    data = .,
    regions = c('counties'),
    exclude = c('AK', 'HI'),
    values = "HPI_binary",
    size = 0,
    colour = "white"
  ) + 
  
  scale_fill_manual(
    values = c(`Present` = "#1696d2",
               `Missing` = "#fdbf11"),
    na.value = "grey") +
  
  labs(
    title = "Housing Price Index is Missing in 319 Counties in Our Data",
    subtitle = glue::glue("We would lose {percent_lost} of sale observations by omitting NA values.\n"),
    fill = "HPI Status",
  ) + 
  
  theme(text = element_text(family = "Lato", size = 25),
        plot.subtitle = element_text(size = 20, face = "italic"),
        legend.position = "top",
        legend.justification = "center",
        legend.spacing = margin(rep(10,4)),
        plot.margin = margin(rep(10,4)))
