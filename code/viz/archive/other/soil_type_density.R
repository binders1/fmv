#================================================================
# Set up ####
#================================================================

## Libraries ####
library(arrow)
library(tidyverse)
library(magrittr)
library(sf)
library(ggtext)
library(RColorBrewer)

## Directories ####
root <- "~/fmv"
cdir <- file.path(root, "code")
ddir <- file.path(root, "data")
clean_dir <- file.path(ddir, "cleaned")
func_dir <- file.path(cdir, "functions")

## Source scripts ####
walk(list.files(func_dir, full.names = TRUE),
     source)
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(cdir, "viz/model/frr/viz_frr_prep.R"))

# Load font ####
font <- "Open Sans"
loadFont(font)


#================================================================
# Import data ####
#================================================================

clean_to_import <- 
  list.files(
    clean_dir, 
    full.names = TRUE
    )

soil_vars <- 
  googlesheets4::range_read(ss = "1AJlJgiMgMQXB9kNKMRuVP6_f5D60-bPmnBMGVYpVPYs",
                            sheet = "New Categories") %>%
  pull(category) %>%
  unique()

loadSoil <- function(file) {
  
  out <-
    read_parquet(file) %>%
    select(sid, x, y, ha, any_of(soil_vars)) %>%
    janitor::clean_names()
  
  return(out)
}

clean_data <-
  map_dfr(
    clean_to_import,
    loadSoil
    )

# Calculate area covered by each soil type by SID

soil_area_data <-
  clean_data %>% 
  mutate(across(!c(sid, x, y, ha), 
                ~ ha * .x,
                .names = "{.col}_ha"),
         .keep = "unused")

rm(clean_data)

# Convert to spatial dataframe
soil_area_sf <-
  soil_area_data %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(5070)) %>%
  rowid_to_column("sid_row") %>%
  mutate(sid_row = as.character(sid_row))

rm(soil_area_data)

#================================================================
# Create tessellation ####
#================================================================

# EPSG:5070 projection of states
state_5070 <- st_transform(us_states, 
                           crs = st_crs(5070))

# Set cell size
cs <- 10e+03

# Make tesselation of CONUS
state_grid <- sf::st_make_grid(state_5070,
                               cellsize = cs,
                               square = FALSE)

# Retrieve hex's that overlap with states
overlap_idx <- 
  st_intersects(state_grid, state_5070) %>%
  map_int(., 
          length) %>% 
  as.logical()

# Filter to only overlapping hex's
state_tessel <-
  state_grid[overlap_idx]

# Trim tessellation to fit state polygon extent 
tessel_crop <-
  st_intersection(state_tessel,
                  state_5070)



#================================================================
# Spatial analysis ####
#================================================================

tessel_sf <-
  tessel_crop %>% 
  tibble(geometry = .) %>%
  rowid_to_column(var = "cell_id") %>%
  st_set_geometry(value = "geometry")


tessel_soil <-

  # create df of just tessel cell id's
  tibble(
    cell_id = tessel_sf$cell_id
    ) %>%
  
  # create list-column of all the sid's contained within each cell
  mutate(
    sid_row = 
      map(
        st_contains(tessel_crop, soil_area_sf), 
        as.character
        ) 
      ) %>% 
  
  # unnest sid's contained within each cell
  unnest(sid_row) %>%
  
  # perform join to attach soil attributes
  left_join(
    soil_area_sf %>% st_drop_geometry(),
    by = "sid_row"
  ) %>%
  
  # group by tessel cell
  group_by(cell_id) %>%
  
  # sum total soil type area within each cell
  summarise(
    across(ends_with("ha"), sum)
  ) %>%
  
  # join cell spatial attributes
  right_join(
    tessel_sf,
    by = "cell_id"
  ) %>% 
  
  # create sf object out of the cells 
  st_set_geometry(value = "geometry") %>%
  
  # filter out any POINT sfc rows from geometry
  filter(!str_detect(st_geometry_type(.), "POINT")) %>%
  
  mutate(across(.fns = replace_na, 0))


#================================================================
# Visualization ####
#================================================================


# States with no prime soil 

no_prime_states <-
  us_states %>%
  filter(abbr %in% c("CA", "CO", "NV"))

ggplot() +
  
  geom_sf(data = state_5070, 
          fill = "lightgrey", colour = NA,
          size = 0.3) +
  
  geom_sf(data = tessel_soil, 
          aes(fill = prime_ha), colour = NA) +
  
  geom_sf(data = no_prime_states,
          colour = NA, fill = "#f2918a") +
  
  geom_sf(data = state_5070, 
          fill = NA,
          size = 0.3) +
  
  scale_fill_gradientn(
    colours = brewer.pal(9, "YlGn"),
    na.value = NA,
    trans = "pseudo_log",
    breaks = c(0, 10^seq(3))
  ) +
  
  labs(
    title = "Prevalence of Prime Soil",
    subtitle = "10km^2 tessellation of the conterminous US",
    fill = "Hectares"
  ) +
  
  guides(
    fill = guide_colorbar(
      barwidth = 0.6,
      barheight = 12,
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  
  coord_sf(crs = st_crs(2163)) +
  
  theme(
    text = element_text(family = font, size = 25),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

