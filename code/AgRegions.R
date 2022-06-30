library(googlesheets4)

ss <- as_sheets_id("https://docs.google.com/spreadsheets/d/1rUfzSfVXLjYnI6hlO-WWR588hKI3NCMiPYHHc1JR2zs/edit#gid=1580896317")

ag_regions <-read_sheet(ss = ss, skip = 2)

ag_regions_key <- ag_regions %>%
  dplyr::select(7) %>%
  slice(1:9) %>%
  separate(col = 1,
           into = c('id','name'),
           sep = "=") %>%
  mutate(id = as.double(id))
  

ag_regions_ref <- ag_regions %>%
  dplyr::select(1:2) %>%
  rename(fips = "Fips", id = 2) %>%
  mutate(fips = ifelse(nchar(fips)==4, 
                       paste0("0",fips),
                       fips),
         state = str_sub(fips, 1, 2)) %>%
  left_join(ag_regions_key)

state_reference <- HPI_county %>%
  
  dplyr::select(fips, state) %>%
  rename(state_name = "state") %>%
  mutate(state = str_sub(fips, 1, 2)) %>%
  dplyr::select(state_name, state) %>%
  filter(!duplicated(state_name)) %>%
  filter(!is.na(state_name))


soil_tbl %>%
  pivot_longer(
    cols = AL:WY,
    names_to = "state_name",
    values_to = "farmlndcl"
  ) %>%
  arrange(state_name) %>%
  filter(!is.na(farmlndcl)) %>%
  left_join(state_reference) %>%
  left_join(ag_regions_ref) %>% 
  filter(!is.na(name)) %>%
  count(name, farmlndcl) %>%
  count(farmlndcl) %>%
  arrange(desc(n),farmlndcl) %>%  
  write_sheet(
    ss = "1AJlJgiMgMQXB9kNKMRuVP6_f5D60-bPmnBMGVYpVPYs",
    sheet = "AgRegion"
  )
  

