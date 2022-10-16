

# Retrieve Soil Codes ####
soil_dir <- file.path(ddir, "ArcResults", "soilcodes")

soil_already_here <- 
  list.files(soil_dir)

## specify names of state soil code csvs not already in folder ####
soilcodes_to_load <- 
  drive_contents %>%
  dplyr::filter(str_detect(name, "^soilcodes.+csv$"),
                !is.element(name, soil_already_here)
  ) %>%
  pull(name)

## download from drive ####
walk(soilcodes_to_load,
     ~ drive_download(.x, 
                      path = file.path(soil_dir, .x),
                      overwrite = TRUE))

soilcode_csvs <- 
  soil_already_here %>%
  sort()

## Find the maximum soil codes across all states ####
nrow_spec <- function(.x) {
  df <- read_csv(file.path(soil_dir, .x),
                 show_col_types = FALSE)
  nrow(df)
}

max_n <- 
  map_int(soilcode_csvs,
          nrow_spec) %>%
  max()


## Collate state soil codes into single df ####
soil_tbl <- tibble(rowid = seq_len(max_n))

for (i in seq_along(soilcode_csvs)) {
  
  state <- str_extract(soilcode_csvs[[i]], "(?<=_)[:upper:]{2}")
  
  file_loc <- file.path(soil_dir, soilcode_csvs[[i]])
  
  tmp <- 
    read_csv(file_loc,
             show_col_types = FALSE) %>%
    arrange(farmlndcl) %>%
    rowid_to_column() %>%
    dplyr::select(rowid, farmlndcl) %>%
    rename(!!state := "farmlndcl")
  
  soil_tbl <- soil_tbl %>%
    left_join(tmp,
              by = "rowid")
  
}

## Count soil types ####
soil_counts <- 
  soil_tbl %>% 
  pivot_longer(
    cols = !rowid,
    names_to = "state",
    values_to = 'farmlndcl') %>%
  dplyr::filter(!is.na(farmlndcl)) %>%
  count(farmlndcl) %>%
  mutate(prop = 
           round(n/length(soilcode_csvs), 2) 
  ) %>%
  arrange(desc(n))
