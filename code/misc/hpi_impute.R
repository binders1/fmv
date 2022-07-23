


## Load County Adjacency df ####
county_adjacency <-
  readr::read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
                  show_col_types = F)


## Load HPI ####
HPI_county <- 
  readr::read_csv("~/fmv/data/HPIcounty.csv",
                  show_col_types = F)

HPI_county %<>% 
  mutate(state = str_sub(fips, 1, 2)) %>%
  filter(!is.element(state, c("02", "15"))) %>%
  select(!state) %>%
  mutate(across(starts_with("HPI_"), 
                ~ .x / HPI_2020)) %>%
  pivot_longer(
    cols = !fips,
    names_to = "year",
    values_to = "HPI",
    names_pattern = "^HPI_(\\d{4})"
  ) %>%
  mutate(year = as.numeric(year))


## Load Home Value ####
setwd("~/fmv/data/homevalue")

all_val <- list.files("~/fmv/data/homevalue")


homeval <- 
  map_dfr(all_val,
          read_parquet)


val_2020 <- 
  homeval %>%
  pivot_wider(
    names_from = year,
    values_from = MEDHOMEVAL,
    names_prefix = "homeval_"
  ) %>%
  select(fips, homeval_2020)


val_est <- 
  HPI_county %>%
  filter(year %in% seq(2010, 2019)) %>%
  arrange(fips, year) %>%
  left_join(val_2020, by = "fips") %>%
  mutate(val_est = HPI*homeval_2020)


val_error <- 
  homeval %>%
  filter(year < 2020) %>%
  left_join(val_est %>%
              select(fips, year, val_est), 
            by = c("fips", "year")) %>%
  mutate(sq_error = (val_est - MEDHOMEVAL)^2)



val_error %>%
  na.omit() %>%
  ggplot(aes(sq_error)) + 
  geom_histogram(alpha = 0.7, fill = msecolors[3]) +
  scale_x_log10(labels = scales::scientific) + 
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 5000)) +
  
  labs(
    title = "MHV Estimates Based on HPI are Distributed Log Normally",
    x = "Sq. Error (log scale)",
    y = "Count"
  ) +
  
  theme(
    text = element_text(family = "Lato", size = 25, colour = "grey30"),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, size = 1, colour = "grey20"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(rep(20,4)),
    axis.ticks.y = element_blank()
  )

val_error %>%
  na.omit() %>%
  mutate(year = fct_inorder(as.character(year))) %>%
  ggplot(aes(year, sqrt(sq_error))) +
  
  geom_boxplot(outlier.colour = "grey30",
               outlier.alpha = 0.5) +
  
  scale_y_log10(labels = scales::comma) +
  
  labs(
    title = "Error from Estimating Home Value with HPI is Consistently Large",
    subtitle = "Root squared error increases with distance from 2020. Points represent county observations.",
    y = "Root Sq. Error (log scale)",
    x = NULL
  ) +
  
  theme(
    text = element_text(family = "Lato", size = 25, colour = "grey30"),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, size = 1, colour = "grey20"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(rep(10,4)),
    axis.ticks.y = element_blank()
  )



lrmse_df <- 
  val_error %>%
  group_by(fips) %>%
  summarise(log_rmse = log(sqrt(mean(sq_error))))


usmap::plot_usmap(
  data = lrmse_df,
  regions = c("counties"),
  exclude = c("AK", "HI"),
  values = "log_rmse",
  size = 0,
) +
  
  scale_fill_gradientn(colors = msecolors,
                       na.value = "lightgrey")+
  
  labs(
    title = "Estimating Home Value with HPI Produces Large Error",
    subtitle = "Med Home Value in county *i* in year *j* was estimated as *MHV^j,i = HPI^j,i x MHV^2020,i*",
    caption = "Sources: US Federal Housing Finance Agency (HPI); American Community Survey (MHV)",
    fill = "Log RMSE"
  ) +
  
  theme(
    text = element_text(size = 25, family = "Lato"),
    plot.subtitle = element_markdown(size = 17),
    plot.caption = element_text(size = 15, face = "italic"),
    legend.background = element_blank()
  )





hpi_missing_fips <- missing_hpi_national %>%
  filter(HPI_binary == 0) %>%
  pull(fips)


ex_fip <- hpi_missing_fips[[191]]

neighbors <- county_adjacency %>%
  filter(fipscounty == ex_fip) %>%
  filter(fipsneighbor != ex_fip) %>%
  pull(fipsneighbor)

reg_df <- homeval %>%
  filter(fips %in% c(ex_fip, neighbors)) %>%
  pivot_wider(
    names_from = fips,
    values_from = MEDHOMEVAL,
    names_prefix = "fips_"
  )

outcome <- paste0("fips_", ex_fip)

predictors <- paste0("fips_", neighbors) %>%
  paste0(., collapse = " + ")

formula <- glue::glue("{outcome} ~ {predictors} + year")

f <- as.formula(formula)

mod <- lm(f, data = reg_df)

predict(mod, )


