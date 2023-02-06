

# ------------ Constants ------------------------ #

nolte2020vars <- 
  read_helper_data("nolte2020vars.csv") %>%
  select(matched_to_gold2022) %>%
  filter(!is.na(matched_to_gold2022)) %>%
  pull()

# Load county adjacencies
county_adjacency <- 
  readr::read_csv(
    "https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv",
    show_col_types = FALSE) %>%
  filter(fipscounty != fipsneighbor)

# Vector of all state abbreviations
all_states <-
  list.files(clean_dir) %>%
  str_extract("[A-Z]{2}(?=\\.pqt$)") %>%
  sort()

