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
  left_join(ag_regions_key) %>%
  mutate(fips = case_when(
    fips == "46113" ~ "46102",
    TRUE ~ as.character(fips)
  ))

