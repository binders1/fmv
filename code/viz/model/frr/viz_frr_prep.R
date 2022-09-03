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


# Set Directories 

ddir <- "~/fmv/data"
mdir <- file.path(ddir, "model")
sdir <- file.path(ddir, "spatial")

state_dir <- file.path(sdir, "state_shp")
county_dir <- file.path(sdir, "county_shp")
aiannh_dir <- file.path(sdir, "aiannh")



frr_sf <- 
  ag_regions_ref %>%
  mutate(
    fips = case_when(
      fips == "46113" ~ "46102",
      TRUE ~ as.character(fips)
    )
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
  arrange(frr_id) %>%
  pull(frr_name) %>%
  unique()

## Function to create single polygon for each FRR ####
frrUnion <- function(data, frr) {
  
  out <-
    
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
    
  
  return(out)
  
}


# If frr_shp doesn't yet exist...
if (!exists("frr_shp")) {
  
  
  # ...map the union function across all FRRs
  frr_shp <-
    map_dfr(frr_names, 
            ~ frrUnion(data = frr_sf, 
                       frr = .x))
  
}


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
    fips = "GEOID"
  )

## AINANH shp ####

aiannh_shp <-
  list.files(aiannh_dir, 
             pattern = "shp$", 
             full.names = TRUE)

aiannh_sf <-
  st_read(aiannh_shp) %>%
  st_crop(., 
          xmin = -130, xmax = -60,
          ymin = 20, ymax = 50) %>%
  st_transform(., crs = st_crs(5070))







