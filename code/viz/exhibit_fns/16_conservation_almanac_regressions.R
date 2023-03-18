
conservation_almanac_regressions <- function() {
  
}

# Mark counties in ffb but left out of nolte_counties.csv

## Read in predict_all for ffb, then mark county as "small"
## if it does not appear in nolte_counties.csv

nolte_counties <-
  read_helper_data("nolte_counties.csv") %>%
  pull(fips)

ffb_counties <-
  loadResults(
    model = "ffb",
    res_type = "predict_all"
  ) %>%
  mutate(
    fips = str_sub(sid, 1, 5)
    ) %>%
  distinct(fips) %>%
  mutate(
    left_out_nolte = !(fips %in% nolte_counties),
    left_out_nolte = as.numeric(left_out_nolte)
      )

# Read in conservation indicators
conserve_by_mech <-
  read_helper_data("conservation_almanac_counts.csv") %>%
  dplyr::filter(!str_detect(fips, "~~")) %>%
  split(.$protmech)

# Fed and Tribal land areas
fed_tribal_area <-
  s.dir %>%
  list.files(
    full.names = TRUE,
    pattern = "^FedTribal"
  ) %>%
  readxl::read_xls() %>%
  select(fips = GEOID, 
         ALAND, AWATER, 
         fed_tribal_km2 = sum_Area_SQUAREKILOMETERS) %>%
  


# Merge with county shapefile
conserve_counties_sf <-
  reduce(
    .x = list(fed_tribal_area, ffb_counties, conserve_by_mech$f),
    .f = full_join,
    by = "fips"
  )


# Normalize conservation indicator by county area
normalize_conserve_measures <-
  conserve_counties_sf %>%
  mutate(
    county_area_km2 = (ALAND + AWATER)/1e+06,
    fed_tribal_prop = fed_tribal_km2 / county_area_km2,
    state = str_sub(fips, 1, 2),
    across(
      .cols = c(acreage, spending, count),
      .fns = ~ .x / fed_tribal_km2,
      .names = "{.col}_per_public_km2"
    )
  ) %>%
  na.omit()

# Run regression
# - outcome = conservation indicator
# - treatment = whether county is small
# - FEs: state

outcome_vars <- 
  c("acreage",
    "spending",
    "count") %>%
  purrr::set_names(nm = c("(1)", "(2)", "(3)"))

conservation_regression <- function(outcome_var) {
  
  fml <- paste0(outcome_var, " ~ left_out_nolte + fed_tribal_prop")
  
  fixest::feols(
    fml = as.formula(fml),
    data = normalize_conserve_measures
  ) 
}

map(
  outcome_vars,
  conservation_regression
) %>%
  fixest::etable()


