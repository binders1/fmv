# Load packages ####
library(sf)
library(ggtext)


# Set up ####
root <- "~/fmv"
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
clean_dir <- file.path(ddir, "cleaned")


# Source spatial files and functions ####
source(file.path(cdir, "functions/sourceFuncs.R"))
sourceFuncs()
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(cdir, "viz/model/frr/viz_frr_prep.R"))

# Load font ####
font <- "Open Sans"
loadFont(font)


# Create tessellation ####

# EPSG:5070 projection of states
state_5070 <- st_transform(us_states, 
                           crs=st_crs(5070))

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

# Load full cleaned data ####

clean_to_load <-
  list.files(clean_dir, 
             full.names = TRUE)

clean_sf <-
  map_dfr(clean_to_load, read_parquet) %>%
  select(sid, x, y) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(5070))

# Combine obs and hex tessellation ####

# Bind predicted obs to hex's
clean_intersect <-
  st_intersects(tessel_crop,
                clean_sf) %>%
  map_int(., length)

# Count obs in each hex
tessel_clean_count <-
  tessel_crop %>%
  st_as_sf() %>%
  mutate(n = clean_intersect) %>%
  st_set_geometry(., "geometry") %>%
  filter(!str_detect(st_geometry_type(.), "POINT"))

# VIZ ####

clean_obs_density <-
  ggplot() +
  
  geom_sf(data = tessel_clean_count, aes(fill = n), size = 0) +
  
  geom_sf(data = state_5070, 
          fill = NA, size = 0.3) +
  
  scale_fill_gradientn(
    colours = c("grey85", brewer.pal(9, "YlOrRd")),
    trans = "pseudo_log",
    breaks = c(0, 10^seq(3))
  ) +
  
  labs(
    fill = "Observed\nSales"
  ) +
  
  guides(
    fill = guide_colorbar(
      barwidth = 0.6,
      barheight = 12,
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  
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

clean_obs_density



nrow(clean_sf) %>% scales::comma()

