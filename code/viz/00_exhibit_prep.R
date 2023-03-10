
# =====================================================
# 01). Load FRR region crosswalk
# =====================================================
frr_key <- 
  read_helper_data("frr_key.csv")

county_frr_crosswalk <- 
  read_helper_data("county_frr_crosswalk.csv")

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
  county_frr_crosswalk %>%
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
frr_union <- function(data, frr) {
  
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
            ~ frr_union(data = frr_sf, frr = .x)
            )
  
}

# Create tessellation of CONUS to use in mapping prediction values
if ("conus_tessel.geojson" %in% list.files(helper_dir)) {
  conus_tessel_sf <- read_sf(file.path(helper_dir, "conus_tessel.geojson"))
} else {
  
  conus_tessel_sf <-
    # Using the CONUS state-level shapefile...
    us_states %>%
    # ...create a custom hex tessellation
    make_us_tessel() %>%
    # Convert to tibble
    tibble(geometry = .) %>%
    # Create unique identifier for each cell
    rowid_to_column(var = "cell_id") %>%
    # Set sf object geometry
    st_set_geometry(value = "geometry")
  
  # Save in file system
  write_sf(
    conus_tessel_sf,
    file.path(helper_dir, "conus_tessel.geojson")
  )
  
}

# =====================================================
# 03). Read in Nolte (2020) Variables
# =====================================================
nolte2020vars <- 
  read_helper_data("nolte2020vars.csv", show_col_types = FALSE) %>%
  dplyr::filter(!is.na(matched_to_gold2022)) %>%
  pull(matched_to_gold2022) %>%
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
                        margin = margin(rep(0, 4))),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", size = 0.3),
    panel.grid.major.x = element_blank(),
    legend.key = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_blank()
  )

