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
          read_parquet) %>%
  pivot_wider(
    names_from = year,
    values_from = MEDHOMEVAL,
    names_prefix = "homeval_"
  )

val_2020 <- 
  homeval %>%
  select(fips, homeval_2020)


val_est <- 
  HPI_county %>%
  filter(year %in% seq(2000, 2009)) %>%
  arrange(fips, year) %>%
  left_join(val_2020, by = "fips") %>%
  mutate(val_est = HPI*homeval_2020) %>%
  select(!c(homeval_2020, HPI)) %>%
  pivot_wider(
    names_from = year,
    values_from = val_est,
    names_prefix = "homeval_"
  ) %>%
  left_join(
    homeval, by = "fips"
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


# predict()



