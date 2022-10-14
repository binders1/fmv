
# =====================================================
# 01). Load FRR region crosswalk
# =====================================================


# TODO: Turn this into a local data file
googlesheets4::gs4_auth("gold1@stolaf.edu")
ss <- as_sheets_id("1rUfzSfVXLjYnI6hlO-WWR588hKI3NCMiPYHHc1JR2zs")

ag_regions <-
  read_sheet(ss = ss, 
             skip = 2)

ag_regions_key <- 
  ag_regions %>%
  dplyr::select(7) %>%
  slice(1:9) %>%
  separate(col = 1,
           into = c('id','frr_name'),
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


# =====================================================
# 02). Load state, county, and FRR spatial data
# =====================================================

# Load spatial files ####

## Set undesired states/territories ####

state_rm <-
  c("02", "15", "60", "66", "69", "72", "78")

## State .shp ####

state_shp <- list.files(state_dir, pattern = "shp$")

us_states <-
  st_read(
    file.path(state_dir, state_shp)
  )

us_states %<>%
  janitor::clean_names() %>%
  select(
    state_fips = "statefp",
    abbr = "stusps"
  ) %>%
  filter(!(state_fips %in% state_rm))


## County shp ####

county_shp <- list.files(county_dir, pattern = "shp$")

us_counties <-
  st_read(
    file.path(county_dir, 
              county_shp)
  )

us_counties %<>%
  filter(!(STATEFP %in% state_rm)) %>%
  select(
    fips = "GEOID",
    ALAND, AWATER
  )




# Create FRR spatial object ####
frr_sf <- 
  ag_regions_ref %>%
  # update old South Dakota county FIPS
  mutate(
    fips = if_else(fips == "46113", "46102", fips)
  ) %>%
  select(fips, id, frr_name) %>%
  left_join(
    us_counties,
    by = "fips"
  )

# Create FRR polygons ####

## Vector of FRR ids to iterate over ####
frr_names <-
  frr_sf %>%
  arrange(id) %>%
  pull(frr_name) %>%
  unique()

## Function to create single polygon for each FRR ####
frrUnion <- function(data, frr) {
  
  data %>%
    
    # subset to only current frr
    filter(frr_name == frr) %>%
    
    # convert to sf object 
    st_as_sf() %>%
    
    # merge all counties within frr 
    st_union() %>%
    tibble() %>%
    
    # add frr name
    mutate(frr_name = as.character(frr)) %>%
    relocate(frr_name) %>%
    
    # convert back to sf object
    st_as_sf() %>%
    
    # name geometry column
    st_set_geometry(value = "geometry")
  
}


# If frr_shp doesn't yet exist...
if (!exists("frr_shp")) {
  
  # ...map the union function across all FRRs
  frr_shp <-
    map_dfr(frr_names, 
            ~ frrUnion(data = frr_sf, 
                       frr = .x))
  
}

# =====================================================
# 03). Set custom colors and theme
# =====================================================
noltevars_path <- file.path(ddir, "nolte2020vars.csv")

nolte2020vars <- 
  read_csv(noltevars_path,
           show_col_types = F) %>% 
  pull() %>%
  c(., "ha", "x45", "y45")

# =====================================================
# 04). Set custom colors and theme
# =====================================================

# Set FRR Colors ####
frr_colors <- c(`Southern Seaboard` = "#A8D3F2", 
                `Eastern Uplands` = "#A5A5A5", 
                `Basin and Range` = "#C3AB1B", 
                `Fruitful Rim` = "#71C276", 
                `Mississippi Portal` = "#AC842B", 
                `Prairie Gateway` = "#EEAB56", 
                `Northern Great Plains` = "#CAC7A1", 
                `Northern Crescent` = "#F3A193", 
                `Heartland` = "#F1EC00")

# custom ggplot theme
fmv_theme <-
  theme(
    text = element_text(size = 12, family = "sans",
                        lineheight = 0.3, margin = margin(rep(0, 4))),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", size = 0.3),
    panel.grid.major.x = element_blank(),
    legend.key = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_blank()
  )

